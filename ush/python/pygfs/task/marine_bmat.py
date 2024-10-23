#!/usr/bin/env python3

import os
import glob
from logging import getLogger
import pygfs.utils.marine_da_utils as mdau

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta,
                    chdir,
                    parse_j2yaml, save_as_yaml,
                    logit,
                    Executable,
                    Task)

from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class MarineBMat(Task):
    """
    Class for global marine B-matrix tasks
    """
    @logit(logger, name="MarineBMat")
    def __init__(self, config):
        """Constructor for marine B-matrix task

        This method will construct the marine B-matrix task object
        This includes:
        - extending the task_config AttrDict to include parameters required for this task
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

        _home_gdas = os.path.join(self.task_config.HOMEgfs, 'sorc', 'gdas.cd')
        _calc_scale_exec = os.path.join(self.task_config.HOMEgfs, 'ush', 'soca', 'calc_scales.py')
        _window_begin = add_to_datetime(self.task_config.current_cycle,
                                        -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle,
                                      to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'PARMsoca': os.path.join(self.task_config.PARMgfs, 'gdas', 'soca'),
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'ENSPERT_RELPATH': _enspert_relpath,
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z."
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        self.jedi = AttrDict()

        # gridgen
        self.jedi['gridgen'] = Jedi(AttrDict(
            {
                'yaml_name': 'gridgen',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_GRIDGEN,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_GRIDGEN,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

        # soca_diagb
        self.jedi['soca_diagb'] = Jedi(AttrDict(
            {
                'yaml_name': 'soca_diagb',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_DIAGB,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_DIAGB,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

        # soca_parameters_diffusion_vt
        self.jedi['soca_parameters_diffusion_vt'] = Jedi(AttrDict(
            {
                'yaml_name': 'soca_parameters_diffusion_vt',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_PARAMETERS_DIFFUSION_VT,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_PARAMETERS_DIFFUSION_VT,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

        # soca_setcorscales
        self.jedi['soca_setcorscales'] = Jedi(AttrDict(
            {
                'yaml_name': 'soca_setcorscales',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_SETCORSCALES,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_SETCORSCALES,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

        # soca_parameters_diffusion_hz
        self.jedi['soca_parameters_diffusion_hz'] = Jedi(AttrDict(
            {
                'yaml_name': 'soca_parameters_diffusion_hz',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_PARAMETERS_DIFFUSION_HZ,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_PARAMETERS_DIFFUSION_HZ,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

        # soca_ensb
        self.jedi['soca_ensb'] = Jedi(AttrDict(
            {
                'yaml_name': 'soca_ensb',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_ENSB,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_ENSB,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

        # soca_ensweights
        self.jedi['soca_ensb'] = Jedi(AttrDict(
            {
                'yaml_name': 'soca_ensb',
                'rundir': self.task_config.DATA,
                'exe_src': self.task_config.JEDIEXE_ENSWEIGHTS,
                'jcb_base_yaml': self.task_config.JCB_BASE_YAML,
                'jcb_algo': self.task_config.JCB_ALGO_ENSWEIGHTS,
                'jcb_algo_yaml': None,
                'jedi_args': None
            }
        ))

    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize a global B-matrix

        This method will initialize a global B-Matrix.
        This includes:
        - staging the deterministic backgrounds
        - staging SOCA fix files
        - staging static ensemble members (optional)
        - staging ensemble members (optional)
        - initializing the soca_vtscales Python script
        - initializing the JEDI applications
        - creating output directories

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage fix files
        logger.info(f"Staging SOCA fix files from {self.task_config.SOCA_INPUT_FIX_DIR}")
        soca_fix_list = parse_j2yaml(self.task_config.SOCA_FIX_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_list).sync()

        # prepare the MOM6 input.nml
        mdau.prep_input_nml(self.task_config)

        # stage backgrounds
        # TODO(G): Check ocean backgrounds dates for consistency
        bkg_list = parse_j2yaml(self.task_config.MARINE_DET_STAGE_BKG_YAML_TMPL, self.task_config)
        FileHandler(bkg_list).sync()

        # stage the soca utility yamls (fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files")
        soca_utility_list = parse_j2yaml(self.task_config.MARINE_UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

        # initialize vtscales python script
        vtscales_config = self.jedi['soca_parameters_diffusion_vt'].render_jcb(self.task_config, 'soca_vtscales')
        save_as_yaml(vtscales_config, os.path.join(self.task_config.DATA, 'soca_vtscales.yaml'))
        FileHandler({'copy': [[os.path.join(self.task_config.CALC_SCALE_EXEC),
                               os.path.join(self.task_config.DATA, 'calc_scales.x')]]}).sync()

        # initialize JEDI applications
        self.jedi['gridgen'].initialize(self.task_config)
        self.jedi['soca_diagb'].initialize(self.task_config)
        self.jedi['soca_parameters_diffusion_vt'].initialize(self.task_config)
        self.jedi['soca_setcorscales'].initialize(self.task_config)
        self.jedi['soca_parameters_diffusion_hz'].initialize(self.task_config)
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            self.jedi['soca_ensb'].initialize(self.task_config)
            self.jedi['soca_ensweights'].initialize(self.task_config)

        # stage ensemble members for the hybrid background error
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            logger.debug(f"Stage ensemble members for the hybrid background error")
            mdau.stage_ens_mem(self.task_config)

        # create the symbolic link to the static B-matrix directory
        link_target = os.path.join(self.task_config.DATAstaticb)
        link_name = os.path.join(self.task_config.DATA, 'staticb')
        if os.path.exists(link_name):
            os.remove(link_name)
        os.symlink(link_target, link_name)

    @logit(logger)
    def execute_vtscales(self: Task) -> None:
        """Execute vertical diffusion coefficients generator

        This method will execute a Python script which generatres the vertical diffusion coefficients
        This includes:
        - constructing the executable object
        - running the executable object

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """
        # compute the vertical correlation scales based on the MLD
        exec_cmd = Executable("python")
        exec_name = os.path.join(self.task_config.DATA, 'calc_scales.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_vtscales.yaml')
        mdau.run(exec_cmd)

    @logit(logger)
    def execute(self, aprun_cmd: str) -> None:
        """Generate the full B-matrix

        This method will generate the full B-matrix according to the configuration.
        This includes:
        - running all JEDI application and Python scripts required to generate the B-matrix

        Parameters
        ----------
        aprun_cmd: str
            String comprising the run command for the JEDI executable.

        Returns
        ----------
        None
        """

        self.jedi['gridgen'].execute(aprun_cmd)  # TODO: This should be optional in case the geometry file was staged
        self.jedi['soca_diagb'].execute(aprun_cmd)
        self.jedi['soca_setcorscales'].execute(aprun_cmd)  # TODO: Make this optional once we've converged on an acceptable set of scales
        self.jedi['soca_parameters_diffusion_hz'].execute(aprun_cmd)  # TODO: Make this optional once we've converged on an acceptable set of scales
        self.execute_vtscales()
        self.jedi['soca_parameters_diffusion_vt'].execute(aprun_cmd)
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            self.jedi['soca_ensb'].execute(self.task_config)  # TODO: refactor this from the old scripts
            self.jedi['soca_ensweights'].execute(self.task_config)  # TODO: refactor this from the old scripts

    @logit(logger)
    def finalize(self: Task) -> None:
        """Finalize the global B-matrix job

        This method will finalize the global B-matrix job.
        This includes:
        - copy the generated static, but cycle dependent background error files to the ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - keep the re-balanced ensemble perturbation files in DATAenspert
        - ...

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """
        # Copy the soca grid if it was created
        grid_file = os.path.join(self.task_config.DATA, 'soca_gridspec.nc')
        if os.path.exists(grid_file):
            logger.info(f"Copying the soca grid file to the ROTDIR")
            FileHandler({'copy': [[grid_file,
                                   os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX, 'soca_gridspec.nc')]]}).sync()

        # Copy the diffusion coefficient files to the ROTDIR
        logger.info(f"Copying the diffusion coefficient files to the ROTDIR")
        diffusion_coeff_list = []
        for diff_type in ['hz', 'vt']:
            src = os.path.join(self.task_config.DATAstaticb, f"{diff_type}_ocean.nc")
            dest = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                                f"{self.task_config.APREFIX}{diff_type}_ocean.nc")
            diffusion_coeff_list.append([src, dest])

        src = os.path.join(self.task_config.DATAstaticb, f"hz_ice.nc")
        dest = os.path.join(self.task_config.COMOUT_ICE_BMATRIX,
                            f"{self.task_config.APREFIX}hz_ice.nc")
        diffusion_coeff_list.append([src, dest])

        FileHandler({'copy': diffusion_coeff_list}).sync()

        # Copy diag B files to ROTDIR
        logger.info(f"Copying diag B files to the ROTDIR")
        diagb_list = []
        window_end_iso = self.task_config.MARINE_WINDOW_END.strftime('%Y-%m-%dT%H:%M:%SZ')

        # ocean diag B
        os.rename(os.path.join(self.task_config.DATAstaticb, f"ocn.bkgerr_stddev.incr.{window_end_iso}.nc"),
                  os.path.join(self.task_config.DATAstaticb, f"ocn.bkgerr_stddev.nc"))
        src = os.path.join(self.task_config.DATAstaticb, f"ocn.bkgerr_stddev.nc")
        dst = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                           f"{self.task_config.APREFIX}ocean.bkgerr_stddev.nc")
        diagb_list.append([src, dst])

        # ice diag B
        os.rename(os.path.join(self.task_config.DATAstaticb, f"ice.bkgerr_stddev.incr.{window_end_iso}.nc"),
                  os.path.join(self.task_config.DATAstaticb, f"ice.bkgerr_stddev.nc"))
        src = os.path.join(self.task_config.DATAstaticb, f"ice.bkgerr_stddev.nc")
        dst = os.path.join(self.task_config.COMOUT_ICE_BMATRIX,
                           f"{self.task_config.APREFIX}ice.bkgerr_stddev.nc")
        diagb_list.append([src, dst])

        FileHandler({'copy': diagb_list}).sync()

        # Copy the ensemble perturbation diagnostics to the ROTDIR
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 3:
            window_middle_iso = self.task_config.MARINE_WINDOW_MIDDLE.strftime('%Y-%m-%dT%H:%M:%SZ')
            weight_list = []
            src = os.path.join(self.task_config.DATA, f"ocn.ens_weights.incr.{window_middle_iso}.nc")
            dst = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                               f"{self.task_config.APREFIX}ocean.ens_weights.nc")
            weight_list.append([src, dst])

            src = os.path.join(self.task_config.DATA, f"ice.ens_weights.incr.{window_middle_iso}.nc")
            dst = os.path.join(self.task_config.COMOUT_ICE_BMATRIX,
                               f"{self.task_config.APREFIX}ice.ens_weights.nc")
            weight_list.append([src, dst])

            # TODO(G): missing ssh_steric_stddev, ssh_unbal_stddev, ssh_total_stddev and steric_explained_variance

            FileHandler({'copy': weight_list}).sync()

        # Copy the YAML files to the OCEAN ROTDIR
        yamls = glob.glob(os.path.join(self.task_config.DATA, '*.yaml'))
        yaml_list = []
        for yaml_file in yamls:
            dest = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                                f"{self.task_config.APREFIX}{os.path.basename(yaml_file)}")
            yaml_list.append([yaml_file, dest])
        FileHandler({'copy': yaml_list}).sync()
