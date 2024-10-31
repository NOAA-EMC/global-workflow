#!/usr/bin/env python3

import os
import glob
from logging import getLogger
import pygfs.utils.marine_da_utils as mdau

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta,
                    chdir,
                    parse_j2yaml,
                    logit,
                    Executable,
                    Task)

logger = getLogger(__name__.split('.')[-1])


class MarineBMat(Task):
    """
    Class for global marine B-matrix tasks
    """
    @logit(logger, name="MarineBMat")
    def __init__(self, config):
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
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z."
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize a global B-matrix

        This method will initialize a global B-Matrix.
        This includes:
        - staging the deterministic backgrounds
        - staging SOCA fix files
        - staging static ensemble members (optional)
        - staging ensemble members (optional)
        - generating the YAML files for the JEDI and GDASApp executables
        - creating output directories
        """
        super().initialize()

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

        # stage the soca utility yamls (gridgen, fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files")
        soca_utility_list = parse_j2yaml(self.task_config.MARINE_UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

        # generate the variance partitioning YAML file
        logger.info(f"Generate variance partitioning YAML file from {self.task_config.BERROR_DIAGB_YAML}")
        diagb_config = parse_j2yaml(path=self.task_config.BERROR_DIAGB_YAML, data=self.task_config)
        diagb_config.save(os.path.join(self.task_config.DATA, 'soca_diagb.yaml'))

        # generate the vertical decorrelation scale YAML file
        logger.info(f"Generate the vertical correlation scale YAML file from {self.task_config.BERROR_VTSCALES_YAML}")
        vtscales_config = parse_j2yaml(path=self.task_config.BERROR_VTSCALES_YAML, data=self.task_config)
        vtscales_config.save(os.path.join(self.task_config.DATA, 'soca_vtscales.yaml'))

        # generate vertical diffusion scale YAML file
        logger.info(f"Generate vertical diffusion YAML file from {self.task_config.BERROR_DIFFV_YAML}")
        diffvz_config = parse_j2yaml(path=self.task_config.BERROR_DIFFV_YAML, data=self.task_config)
        diffvz_config.save(os.path.join(self.task_config.DATA, 'soca_parameters_diffusion_vt.yaml'))

        # generate the horizontal diffusion YAML files
        if True:  # TODO(G): skip this section once we have optimized the scales
            # stage the correlation scale configuration
            logger.info(f"Generate correlation scale YAML file from {self.task_config.BERROR_HZSCALES_YAML}")
            FileHandler({'copy': [[self.task_config.BERROR_HZSCALES_YAML,
                                   os.path.join(self.task_config.DATA, 'soca_setcorscales.yaml')]]}).sync()

            # generate horizontal diffusion scale YAML file
            logger.info(f"Generate horizontal diffusion scale YAML file from {self.task_config.BERROR_DIFFH_YAML}")
            diffhz_config = parse_j2yaml(path=self.task_config.BERROR_DIFFH_YAML, data=self.task_config)
            diffhz_config.save(os.path.join(self.task_config.DATA, 'soca_parameters_diffusion_hz.yaml'))

        # hybrid EnVAR case
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            # stage ensemble membersfiles for use in hybrid background error
            logger.debug(f"Stage ensemble members for the hybrid background error")
            mdau.stage_ens_mem(self.task_config)

            # generate ensemble recentering/rebalancing YAML file
            logger.debug("Generate ensemble recentering YAML file")
            ensrecenter_config = parse_j2yaml(path=self.task_config.BERROR_ENS_RECENTER_YAML, data=self.task_config)
            ensrecenter_config.save(os.path.join(self.task_config.DATA, 'soca_ensb.yaml'))

            # generate ensemble weights YAML file
            logger.debug("Generate hybrid-weigths YAML file")
            hybridweights_config = parse_j2yaml(path=self.task_config.BERROR_HYB_WEIGHTS_YAML, data=self.task_config)
            hybridweights_config.save(os.path.join(self.task_config.DATA, 'soca_ensweights.yaml'))

        # create the symbolic link to the static B-matrix directory
        link_target = os.path.join(self.task_config.DATAstaticb)
        link_name = os.path.join(self.task_config.DATA, 'staticb')
        if os.path.exists(link_name):
            os.remove(link_name)
        os.symlink(link_target, link_name)

    @logit(logger)
    def gridgen(self: Task) -> None:
        # link gdas_soca_gridgen.x
        mdau.link_executable(self.task_config, 'gdas_soca_gridgen.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_gridgen.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('gridgen.yaml')

        mdau.run(exec_cmd)

    @logit(logger)
    def variance_partitioning(self: Task) -> None:
        # link the variance partitioning executable, gdas_soca_diagb.x
        mdau.link_executable(self.task_config, 'gdas_soca_diagb.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_diagb.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_diagb.yaml')

        mdau.run(exec_cmd)

    @logit(logger)
    def horizontal_diffusion(self: Task) -> None:
        """Generate the horizontal diffusion coefficients
        """
        # link the executable that computes the correlation scales, gdas_soca_setcorscales.x,
        # and prepare the command to run it
        mdau.link_executable(self.task_config, 'gdas_soca_setcorscales.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_setcorscales.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_setcorscales.yaml')

        # create a files containing the correlation scales
        mdau.run(exec_cmd)

        # link the executable that computes the correlation scales, gdas_soca_error_covariance_toolbox.x,
        # and prepare the command to run it
        mdau.link_executable(self.task_config, 'gdas_soca_error_covariance_toolbox.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_error_covariance_toolbox.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_parameters_diffusion_hz.yaml')

        # compute the coefficients of the diffusion operator
        mdau.run(exec_cmd)

    @logit(logger)
    def vertical_diffusion(self: Task) -> None:
        """Generate the vertical diffusion coefficients
        """
        # compute the vertical correlation scales based on the MLD
        FileHandler({'copy': [[os.path.join(self.task_config.CALC_SCALE_EXEC),
                               os.path.join(self.task_config.DATA, 'calc_scales.x')]]}).sync()
        exec_cmd = Executable("python")
        exec_name = os.path.join(self.task_config.DATA, 'calc_scales.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_vtscales.yaml')
        mdau.run(exec_cmd)

        # link the executable that computes the correlation scales, gdas_soca_error_covariance_toolbox.x,
        # and prepare the command to run it
        mdau.link_executable(self.task_config, 'gdas_soca_error_covariance_toolbox.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_soca_error_covariance_toolbox.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_parameters_diffusion_vt.yaml')

        # compute the coefficients of the diffusion operator
        mdau.run(exec_cmd)

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
        mdau.link_executable(self.task_config, 'gdas_ens_handler.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_ens_handler.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_ensb.yaml')

        # generate the ensemble perturbations
        mdau.run(exec_cmd)

    @logit(logger)
    def hybrid_weight(self: Task) -> None:
        """Generate the hybrid weights for the 3DEnVAR

        This method will generate the 3D fields hybrid weights for the 3DEnVAR for each
        variables.
        TODO(G): Currently implemented for the specific case of the static ensemble members only
        """
        mdau.link_executable(self.task_config, 'gdas_socahybridweights.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_socahybridweights.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_ensweights.yaml')

        # compute the ensemble weights
        mdau.run(exec_cmd)

    @logit(logger)
    def execute(self: Task) -> None:
        """Generate the full B-matrix

        This method will generate the full B-matrix according to the configuration.
        """
        chdir(self.task_config.DATA)
        self.gridgen()                 # TODO: This should be optional in case the geometry file was staged
        self.variance_partitioning()
        self.horizontal_diffusion()    # TODO: Make this optional once we've converged on an acceptable set of scales
        self.vertical_diffusion()
        # hybrid EnVAR case
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            self.ensemble_perturbations()  # TODO: refactor this from the old scripts
            self.hybrid_weight()           # TODO: refactor this from the old scripts

    @logit(logger)
    def finalize(self: Task) -> None:
        """Finalize the global B-matrix job

        This method will finalize the global B-matrix job.
        This includes:
        - copy the generated static, but cycle dependent background error files to the ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - keep the re-balanced ensemble perturbation files in DATAenspert
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
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
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
