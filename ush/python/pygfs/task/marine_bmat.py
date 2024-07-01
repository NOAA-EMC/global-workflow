#!/usr/bin/env python3

import f90nml
import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any
import xarray as xr
import calc_scales
import subprocess

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta, to_YMDH,
                    chdir,
                    parse_j2yaml, save_as_yaml,
                    logit,
                    Executable,
                    WorkflowException,
                    Task)

logger = getLogger(__name__.split('.')[-1])


class MarineBMat(Task):
    """
    Class for global marine B-matrix tasks
    """
    @logit(logger, name="MarineBMat")
    def __init__(self, config):
        super().__init__(config)

        _res_anl = self.task_config.OCNRES
        _gcyc_str = str(self.task_config.gcyc).zfill(2)
        _cyc_str = str(self.task_config.cyc).zfill(2)
        _home_gdas = os.path.join(self.task_config.HOMEgfs, 'sorc', 'gdas.cd')
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _jedi_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atmvar.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'gcyc_str': _gcyc_str,
                'cyc_str': _cyc_str,
                'HOMEgdas': _home_gdas,
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'BERROR_YAML_DIR': os.path.join(_home_gdas, 'parm', 'soca', 'berror'),
                'GRID_GEN_YAML': os.path.join(_home_gdas, 'parm', 'soca', 'gridgen', 'gridgen.yaml')
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)


    @logit(logger)
    def _run(self, exec_cmd):
        """Run the executable command
           TODO: Move this method somewhere else
        """
        logger.info(f"Executing {exec_cmd}")

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass


    @logit(logger)
    def _link_executable(self, exe_name: str) -> None:
        """Link the executable to the DATA directory
           TODO: Move this method somewhere else
        """
        logger.info(f"Link executable {exe_name}")
        logger.warn("Linking is not permitted per EE2.")
        exe_src = os.path.join(self.task_config.EXECgfs, exe_name)
        exe_dest = os.path.join(self.task_config.DATA, exe_name)
        if os.path.exists(exe_dest):
            os.remove(exe_dest)
        os.symlink(exe_src, exe_dest)


    @logit(logger)
    def _prep_input_nml(self) -> None:
        """Prepare the input.nml file
           TODO: Move this method somewhere else
        """
        # stage input.nml
        mom_input_nml_tmpl_src = os.path.join(self.task_config.HOMEgdas, 'parm', 'soca', 'fms', 'input.nml')
        mom_input_nml_tmpl = os.path.join(self.task_config.DATA, 'mom_input.nml.tmpl')
        FileHandler({'copy': [[mom_input_nml_tmpl_src, mom_input_nml_tmpl]]}).sync()

        # swap date and stacksize
        domain_stack_size = self.task_config.DOMAIN_STACK_SIZE
        ymdhms = [int(s) for s in self.task_config.MARINE_WINDOW_END.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
        with open(mom_input_nml_tmpl, 'r') as nml_file:
            nml = f90nml.read(nml_file)
            nml['ocean_solo_nml']['date_init'] = ymdhms
            nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
            nml.write('mom_input.nml')


    @logit(logger)
    def _cice_hist2fms(self, input_filename, output_filename) -> None:
        """ Reformat the CICE history file to be read by SOCA/FMS
        Simple reformatting utility to allow soca/fms to read the CICE history files
        """

        # open the CICE history file
        ds = xr.open_dataset(input_filename)

        if 'aicen' in ds.variables and 'hicen' in ds.variables and 'hsnon' in ds.variables:
            logger.info(f"*** Already reformatted, skipping.")
            return

        # rename the dimensions to xaxis_1 and yaxis_1
        ds = ds.rename({'ni': 'xaxis_1', 'nj': 'yaxis_1'})

        # rename the variables
        ds = ds.rename({'aice_h': 'aicen', 'hi_h': 'hicen', 'hs_h': 'hsnon'})

        # Save the new netCDF file
        ds.to_netcdf(output_filename, mode='w')


    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize a global B-matrix

        This method will initialize a global B-Matrix.
        This includes:
        - staging the deterministic backgrounds (middle of window)
        - staging SOCA fix files
        - staging static ensemble members (optional)
        - staging ensemble members (optional)
        - generating the YAML files for the JEDI and GDASApp executables
        - creating output directories
        """
        super().initialize()

        # stage fix files
        logger.info(f"Staging SOCA fix files from {self.task_config.SOCA_INPUT_FIX_DIR}")
        newdirs = [ os.path.join(self.task_config.DATA, 'INPUT') ]
        FileHandler({'mkdir': newdirs}).sync()
        soca_fix_list = parse_j2yaml(self.task_config.SOCA_FIX_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_list).sync()

        # prepare the MOM6 input.nml
        self._prep_input_nml()

        # stage a single background
        # TODO: check if we only need 1 deterministic background
        # TODO: when we decide to move this task to the end of the previous cycle,
        #       the bkg will have to come from HISTORY not HISTORY_PREV
        ocn_bkg_dir = self.task_config.COMIN_OCEAN_HISTORY_PREV
        ice_bkg_dir = self.task_config.COMIN_ICE_HISTORY_PREV
        logger.info("Staging background files from {ocn_bkg_dir} and {ice_bkg_dir}")
        bkg_list = []
        bkg_list.append([os.path.join(ocn_bkg_dir, f"gdas.ocean.t{self.task_config.gcyc_str}z.inst.f009.nc"),
                         os.path.join(self.task_config.DATA, "ocn.bkg.nc")])
        bkg_list.append([os.path.join(ice_bkg_dir, f"gdas.ice.t{self.task_config.gcyc_str}z.inst.f009.nc"),
                         os.path.join(self.task_config.DATA, "ice.bkg.nc")])
        FileHandler({'copy': bkg_list}).sync()
        self._cice_hist2fms("ice.bkg.nc", "ice.bkg.nc")

        # Copy MOM6 restart
        # TODO: check if we can combine this with the step above
        logger.info(f"Linking MOM6 restart to ocn.bkg.nc")
        rst_list = []
        rst_list.append([os.path.join(self.task_config.DATA, "ocn.bkg.nc"),
                         os.path.join(self.task_config.DATA, "INPUT", "MOM.res.nc")])
        rst_list.append([os.path.join(self.task_config.DATA, "ice.bkg.nc"),
                         os.path.join(self.task_config.DATA, "INPUT", "cice.res.nc")])
        FileHandler({'copy': rst_list}).sync()

        # stage the grid generation yaml
        FileHandler({'copy': [[self.task_config.GRID_GEN_YAML,
                               os.path.join(self.task_config.DATA, 'gridgen.yaml')]]}).sync()

        # generate the variance partitioning YAML file
        logger.debug("Generate variance partitioning YAML file")
        diagb_config = parse_j2yaml(path=os.path.join(self.task_config.BERROR_YAML_DIR, 'soca_diagb.yaml.j2'),
                                    data=self.task_config)
        diagb_config.save(os.path.join(self.task_config.DATA, 'soca_diagb.yaml'))

        # generate the vertical decorrelation scale YAML file
        logger.debug("Generate the vertical correlation scale YAML file")
        vtscales_config = parse_j2yaml(path=os.path.join(self.task_config.BERROR_YAML_DIR, 'soca_vtscales.yaml.j2'),
                                       data=self.task_config)
        vtscales_config.save(os.path.join(self.task_config.DATA, 'soca_vtscales.yaml'))

        # generate vertical diffusion scale YAML file
        logger.debug("Generate vertical diffusion YAML file")
        diffvz_config = parse_j2yaml(path=os.path.join(self.task_config.BERROR_YAML_DIR, 'soca_parameters_diffusion_vt.yaml.j2'),
                                     data=self.task_config)
        diffvz_config.save(os.path.join(self.task_config.DATA, 'soca_parameters_diffusion_vt.yaml'))


        # generate the horizontal diffusion YAML files
        if True: #task_config.COMPUTE_HORIZ_DIFF:
            # stage the correlation scale configuration
            logger.debug("Generate correlation scale YAML file")
            FileHandler({'copy': [[os.path.join(self.task_config.BERROR_YAML_DIR, 'soca_setcorscales.yaml'),
                                   os.path.join(self.task_config.DATA, 'soca_setcorscales.yaml')]]}).sync()

            # generate horizontal diffusion scale YAML file
            logger.debug("Generate horizontal diffusion scale YAML file")
            diffhz_config = parse_j2yaml(path=os.path.join(self.task_config.BERROR_YAML_DIR, 'soca_parameters_diffusion_hz.yaml.j2'),
                                         data=self.task_config)
            diffhz_config.save(os.path.join(self.task_config.DATA, 'soca_parameters_diffusion_hz.yaml'))

        # hybrid EnVAR case
        if True:  #self.task_config.DOHYBVAR:
            # stage ensemble membersfiles for use in hybrid background error
            logger.debug("Stage ensemble files for DOHYBVAR {self.task_config.DOHYBVAR}")

            # generate ensemble recentering YAML file
            logger.debug("Generate ensemble recentering YAML file: {self.task_config.abcd_yaml}")

            # generate hybrid weights YAML file
            logger.debug("Generate ensemble recentering YAML file: {self.task_config.abcd_yaml}")

        # need output dir for ensemble perturbations and static B-matrix
        logger.debug("Create empty output [ensb, diagb] directories to receive output from executables")
        newdirs = [
            os.path.join(self.task_config.DATA, 'ensb'),
            os.path.join(self.task_config.DATA, 'diagb'),
        ]
        FileHandler({'mkdir': newdirs}).sync()


    @logit(logger)
    def gridgen(self: Task) -> None:

        # link gdas_soca_gridgen.x
        self._link_executable('gdas_soca_gridgen.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_gridgen.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('gridgen.yaml')

        self._run(exec_cmd)

    @logit(logger)
    def variance_partitioning(self: Task) -> None:

        # link the variance partitioning executable, gdas_soca_diagb.x
        self._link_executable('gdas_soca_diagb.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_diagb.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_diagb.yaml')

        self._run(exec_cmd)

    @logit(logger)
    def horizontal_diffusion(self: Task) -> None:
        """Generate the horizontal diffusion coefficients
        """
        # link the executable that computes the correlation scales, gdas_soca_setcorscales.x,
        # and prepare the command to run it
        self._link_executable('gdas_soca_setcorscales.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_setcorscales.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_setcorscales.yaml')

        # create a files containing the correlation scales
        self._run(exec_cmd)

        # link the executable that computes the correlation scales, gdas_soca_error_covariance_toolbox.x,
        # and prepare the command to run it
        self._link_executable('gdas_soca_error_covariance_toolbox.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_error_covariance_toolbox.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_parameters_diffusion_hz.yaml')

        # compute the coefficients of the diffusion operator
        self._run(exec_cmd)

    @logit(logger)
    def vertical_diffusion(self: Task) -> None:
        """Generate the vertical diffusion coefficients
        """
        # compute the vertical correlation scales based on the MLD
        calc_scales.run('soca_vtscales.yaml')

        # link the executable that computes the correlation scales, gdas_soca_error_covariance_toolbox.x,
        # and prepare the command to run it
        self._link_executable('gdas_soca_error_covariance_toolbox.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_error_covariance_toolbox.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_parameters_diffusion_vt.yaml')

        # compute the coefficients of the diffusion operator
        self._run(exec_cmd)


    @logit(logger)
    def ensemble_perturbations(self: Task) -> None:
        """Generate the 3D ensemble of perturbation for the 3DEnVAR

        This method will generate ensemble perturbations re-balanced w.r.t the
        deterministic background.
        This includes:
        - computing a storing the unbalanced ensemble perturbations' statistics
        - recentering the ensemble members around the deterministic background and
          accounting for the nonlinear steric recentering
        - saving the recentered ensemble statistics
        """
        # gdas_ens_handler.x

    @logit(logger)
    def hybrid_weight(self: Task) -> None:
        """Generate the hybrid weights for the 3DEnVAR

        This method will generate the 3D fields hybrid weights for the 3DEnVAR for each
        variables.
        TODO(G): Currently implemented for the specific case of the static ensemble members only
        """
        # gdas_socahybridweights.x

    @logit(logger)
    def execute(self: Task) -> None:
        """Generate the full B-matrix

        This method will generate the full B-matrix according to the configuration.
        """
        chdir(self.task_config.DATA)
        self.initialize()
        self.gridgen()                 # TODO: This should be optional in case the geometry file was staged
        self.variance_partitioning()
        self.horizontal_diffusion()    # TODO: Make this optional once we've converged on an acceptable set of scales
        self.vertical_diffusion()
        self.ensemble_perturbations()  # TODO: refactor this from the old scripts
        self.hybrid_weight()           # TODO: refactor this from the old scripts
        self.finalize()

    @logit(logger)
    def finalize(self: Task) -> None:
        """Finalize the global B-matrix job

        This method will finalize the global B-matrix job.
        This includes:
        - copy the generated static, but cycle dependent background error files to the ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - keep the re-balanced ensemble perturbation files in the DATA/??? directory
        - ...

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
            src = os.path.join(self.task_config.DATA, f"{diff_type}_ocean.nc")
            dest = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                                f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.{diff_type}_ocean.nc")
            diffusion_coeff_list.append([src, dest])

        src = os.path.join(self.task_config.DATA, f"hz_ice.nc")
        dest = os.path.join(self.task_config.COMOUT_ICE_BMATRIX,
                            f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.hz_ice.nc")
        diffusion_coeff_list.append([src, dest])

        FileHandler({'copy': diffusion_coeff_list}).sync()

        # Copy diag B files to ROTDIR
        logger.info(f"Copying diag B files to the ROTDIR")
        diagb_list = []
        window_end_iso = self.task_config.MARINE_WINDOW_END.strftime('%Y-%m-%dT%H:%M:%SZ')

        # ocean diag B
        src = os.path.join(self.task_config.DATA, 'diagb', f"ocn.bkgerr_stddev.incr.{window_end_iso}.nc")
        dst = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                           f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.ocean.bkgerr_stddev.nc")
        diagb_list.append([src, dst])

        # ice diag B
        src = os.path.join(self.task_config.DATA, 'diagb', f"ice.bkgerr_stddev.incr.{window_end_iso}.nc")
        dst = os.path.join(self.task_config.COMOUT_ICE_BMATRIX,
                           f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.ice.bkgerr_stddev.nc")
        diagb_list.append([src, dst])

        FileHandler({'copy': diagb_list}).sync()

        # Copy the YAML files to the OCEAN ROTDIR
        yamls = glob.glob(os.path.join(self.task_config.DATA, '*.yaml'))
        yaml_list = []
        for yaml_file in yamls:
            dest = os.path.join(self.task_config.COMOUT_OCEAN_BMATRIX,
                                f"{self.task_config.RUN}.t{self.task_config.cyc:02d}.{os.path.basename(yaml_file)}")
            yaml_list.append([yaml_file, dest])
        FileHandler({'copy': yaml_list}).sync()
