#!/usr/bin/env python3

import copy
import os
from logging import getLogger
import pygfs.utils.marine_da_utils as mdau
import glob
import re
import netCDF4
from multiprocessing import Process
import subprocess
import yaml
from jcb import render
from pygfs.jedi import Jedi

from wxflow import (AttrDict, FileHandler, Task,
                    add_to_datetime, to_fv3time, to_isotime, to_timedelta, to_YMD,
                    parse_j2yaml, parse_yaml, save_as_yaml,
                    logit,
                    Template, TemplateConstants, YAMLFile)

logger = getLogger(__name__.split('.')[-1])


class MarineAnalysis(Task):
    """
    Class for global marine analysis tasks
    """
    @logit(logger, name="MarineAnalysis")
    def __init__(self, config):
        super().__init__(config)
        _calc_scale_exec = os.path.join(self.task_config.HOMEgfs, 'ush', 'soca', 'calc_scales.py')
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # compute the relative path from self.task_config.DATA to self.task_config.DATAens
        if self.task_config.NMEM_ENS > 0:
            _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)
        else:
            _enspert_relpath = None

        # Determine background error model
        if self.task_config.NMEM_ENS >= 2:
            _berror_model = 'marine_background_error_hybrid_diffusion_diffusion'
        else:
            _berror_model = 'marine_background_error_static_diffusion'

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'PARMsoca': os.path.join(self.task_config.PARMgfs, 'gdas', 'soca'),
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'MARINE_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'MARINE_WINDOW_BEGIN_ISO': to_isotime(_window_begin),
                'MARINE_WINDOW_MIDDLE_ISO': to_isotime(self.task_config.current_cycle),
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'berror_model': _berror_model,
                'obs_list': ['adt_rads_all'],
                'MOM6_LEVS': mdau.get_mom6_levels(str(self.task_config.OCNRES).zfill(3)),
                'app_path_observations': self.task_config.MARINE_JCB_GDAS_OBS
            }
        )

        # Extend task_config with local_dict
        self.task_config.update(local_dict)

        expected_keys = ['var', 'socaincr2mom6', 'soca_2cice_global', 'soca_diag_stats']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML_ANALYSIS, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize the marine analysis

        This method will initialize the marine analysis.
        This includes:
        - staging the deterministic backgrounds (middle of window)
        - staging SOCA fix files
        - staging static ensemble members (optional)
        - staging ensemble members (optional)
        - generating the YAML files for the JEDI and GDASApp executables
        - creating output directories
        """

        # prepare the directory structure to run SOCA
        self._prep_scratch_dir()

        # fetch observations from COMROOT
        # TODO(G.V. or A.E.): Keep a copy of the obs in the scratch fs after the obs prep job
        self._fetch_observations()

        # stage the ocean and ice backgrounds for FGAT
        bkg_list = parse_j2yaml(self.task_config.MARINE_DET_STAGE_BKG_YAML_TMPL, self.task_config)
        FileHandler(bkg_list).sync()

        # stage the soca grid
        FileHandler({'copy': [[os.path.join(self.task_config.COMIN_OCEAN_BMATRIX,
                                            'soca_gridspec.nc'),
                               os.path.join(self.task_config.DATA,
                                            'soca_gridspec.nc')]]}).sync()

        # link the flow dependent static B resources from the B-matrix task of the same cycle
        os.symlink('../staticb', 'staticb')

        # hybrid EnVAR case
        if self.task_config.DOHYBVAR_OCN == "YES" or self.task_config.NMEM_ENS >= 2:
            # stage the ensemble weights
            logger.debug(f"Stage ensemble weights for the hybrid background error")
            FileHandler({'copy': [[os.path.join(self.task_config.COMIN_OCEAN_BMATRIX,
                                                f'{self.task_config.APREFIX}ocean.ens_weights.nc'),
                                   os.path.join(self.task_config.DATA,
                                                'ocean.ens_weights.nc')],
                                  [os.path.join(self.task_config.COMIN_ICE_BMATRIX,
                                                f'{self.task_config.APREFIX}ice.ens_weights.nc'),
                                   os.path.join(self.task_config.DATA,
                                                'ice.ens_weights.nc')]]}).sync()

        # Generate background list
        self.task_config.marine_pseudo_model_states = mdau.gen_bkg_list(bkg_path='./bkg',
                                                                        window_begin=self.task_config.MARINE_WINDOW_BEGIN)

        # make a copy of the CICE6 restart
        # set the restart date, dependent on the cycling type
        if self.task_config.DOIAU:
            # forecast initialized at the begining of the DA window
            fcst_begin = self.task_config.MARINE_WINDOW_BEGIN_ISO
            rst_date = self.task_config.MARINE_WINDOW_BEGIN.strftime('%Y%m%d.%H%M%S')
        else:
            # forecast initialized at the middle of the DA window
            fcst_begin = self.task_config.MARINE_WINDOW_MIDDLE_ISO
            rst_date = self.task_config.MARINE_WINDOW_MIDDLE.strftime('%Y%m%d.%H%M%S')
        ice_rst = os.path.join(self.task_config.COMIN_ICE_RESTART_PREV, f'{rst_date}.cice_model.res.nc')
        ice_rst_ana = os.path.join(self.task_config.DATA, 'Data', rst_date + '.cice_model.res.nc')
        FileHandler({'copy': [[ice_rst, ice_rst_ana]]}).sync()

        # Write obs_list_short
        save_as_yaml(parse_obs_list_file(self.task_config.MARINE_OBS_LIST_YAML), 'obs_list_short.yaml')
        os.environ['OBS_LIST_SHORT'] = 'obs_list_short.yaml'

        # initialize JEDI applications
        self.jedi_dict['var'].initialize(self.task_config)

        # TEST
        self.task_config.obs_variables = {}
        for obs_space in self.jedi_dict['var'].jedi_config.input_config['cost function']['observations']['observers']:
            self.task_config.obs_variables[obs_space['obs space']['name']] = obs_space['obs space']['simulated variables'][0]
#        print('foo', self.task_config.obs_variables)

        self.jedi_dict['socaincr2mom6'].initialize(self.task_config)
        self.jedi_dict['soca_2cice_global'].initialize(self.task_config)
        self.jedi_dict['soca_diag_stats'].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of marine analysis

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[jedi_dict_key].execute()

    @logit(logger)
    def _fetch_observations(self: Task) -> None:
        """Fetch observations from COMIN_OBS

        This method will fetch the observations for the cycle and check the
        list against what is available for the cycle.
        """

        # get the list of observations

        # "observations" is expected by later JCB code to populate it with config info,
        # but the obs_list as such is needed later
        self.task_config.observations = parse_j2yaml(self.task_config.MARINE_OBS_LIST_YAML, self.task_config)['observations']
        self.task_config.obs_list = self.task_config.observations

        obsconfigfile = os.path.join(self.task_config['PARMgfs'], 'gdas/soca/obs/obs_list_base_yaml.j2')
        self.task_config.observations = parse_j2yaml(obsconfigfile, self.task_config)

        obs_files = []

        for observer in self.task_config['observations']['observers']:
            filename = f"{self.task_config.OPREFIX}{observer['obs space']['name'].lower()}.{to_YMD(self.task_config.PDY)}{self.task_config.cyc:02d}.nc4"
            logger.info(f"******** {filename}")
            obs_files.append(filename)

        obs_files_to_copy = []

        # copy obs from COM_OBS to DATA/obs
        for obs_file in obs_files:
            logger.info(f"******* {obs_file}")
            obs_src = os.path.join(self.task_config.COM_OBS, obs_file)
            obs_dst = os.path.join(self.task_config.DATA, 'obs', obs_file)
            logger.info(f"******* {obs_src}")
            if os.path.exists(obs_src):
                logger.info(f"******* fetching {obs_file}")
                obs_files_to_copy.append([obs_src, obs_dst])
            else:
                logger.info(f"******* {obs_file} is not in the database")

        FileHandler({'copy': obs_files_to_copy}).sync()

    @logit(logger)
    def _prep_scratch_dir(self: Task) -> None:
        """Create and stage all the resources needed to run SOCA/JEDI, including the necesssary
           directory structure to run the SOCA variational application
        """
        logger.info(f"---------------- Setup runtime environement")

        anl_dir = self.task_config.DATA

        # create analysis directories
        diags = os.path.join(anl_dir, 'diags')            # output dir for soca DA obs space
        obs_in = os.path.join(anl_dir, 'obs')             # input      "           "
        anl_out = os.path.join(anl_dir, 'Data')           # output dir for soca DA
        FileHandler({'mkdir': [diags, obs_in, anl_out]}).sync()

        # stage fix files
        logger.info(f"Staging SOCA fix files from {self.task_config.SOCA_INPUT_FIX_DIR}")
        soca_fix_list = parse_j2yaml(self.task_config.SOCA_FIX_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_list).sync()

        # prepare the deterministic MOM6 input.nml
        mdau.prep_input_nml(self.task_config)

        # prepare the input.nml for the analysis geometry
        mdau.prep_input_nml(self.task_config, output_nml="./anl_geom/mom_input.nml",
                            simple_geom=True, mom_input="./anl_geom/MOM_input")

        # stage the soca utility yamls (gridgen, fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files from {self.task_config.PARMsoca}")
        soca_utility_list = parse_j2yaml(self.task_config.MARINE_UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

    @logit(logger)
    def finalize(self: Task) -> None:
        """Finalize the marine analysis job
           This method saves the results of the deterministic variational analysis to the COMROOT
        """

        def list_all_files(dir_in, dir_out, wc='*', fh_list=[]):
            files = glob.glob(os.path.join(dir_in, wc))
            for file_src in files:
                file_dst = os.path.join(dir_out, os.path.basename(file_src))
                fh_list.append([file_src, file_dst])
            return fh_list

        # variables of convenience
        bcyc = str(self.task_config.MARINE_WINDOW_BEGIN.hour).zfill(2)

        logger.info(f"---------------- Copy from RUNDIR to COMOUT")

        post_file_list = []

        # Make a copy the IAU increment
        post_file_list.append([os.path.join(self.task_config.DATA,
                                            'ocn.inc.nc'),
                               os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                            f'{self.task_config.APREFIX}ocninc.nc')])

        domains = ['ocn', 'ice']
        for domain in domains:
            '''
            # Copy of the diagonal of the background error for the cycle
            post_file_list.append([os.path.join(self.task_config.DATA,
                                                f'{domain}.bkgerr_stddev.incr.{self.task_config.MARINE_WINDOW_MIDDLE_ISO}.nc'),
                                   os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                                f'{self.task_config.APREFIX}{domain}.bkgerr_stddev.nc')])

            # Copy the recentering error
            if self.task_config.NMEM_ENS > 2:
                post_file_list.append([os.path.join(self.task_config.DATA, 'static_ens',
                                                    f'{domain}.ssh_recentering_error.incr.{self.task_config.MARINE_WINDOW_BEGIN_ISO}.nc'),
                                       os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                                    f'{self.task_config.APREFIX}{domain}.recentering_error.nc')])
            '''

            # Copy the ice and ocean increments
            post_file_list.append([os.path.join(self.task_config.DATA, 'Data',
                                                f'{domain}.3dvarfgat_pseudo.incr.{self.task_config.MARINE_WINDOW_MIDDLE_ISO}.nc'),
                                   os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                                f'{self.task_config.APREFIX}{domain}.incr.nc')])

            # Copy the analysis at the start of the window
            post_file_list.append([os.path.join(self.task_config.DATA, 'Data',
                                                f'{domain}.3dvarfgat_pseudo.an.{self.task_config.MARINE_WINDOW_MIDDLE_ISO}.nc'),
                                   os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                                f'{self.task_config.APREFIX}{domain}ana.nc')])

        # Copy soca2cice ice increment
        post_file_list.append([os.path.join(self.task_config.DATA, 'Data',
                                            f'ice.soca2cice.incr.{self.task_config.MARINE_WINDOW_BEGIN_ISO}.nc'),
                              os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                           f'{self.task_config.APREFIX}ice.incr.postproc.nc')])

        # Copy of the ssh diagnostics
        if self.task_config.NMEM_ENS > 2:
            for string in ['ssh_steric_stddev', 'ssh_unbal_stddev', 'ssh_total_stddev', 'steric_explained_variance']:
                post_file_list.append([os.path.join(self.task_config.DATA, 'staticb',
                                                    f'ocn.{string}.incr.{self.task_config.MARINE_WINDOW_BEGIN_ISO}.nc'),
                                       os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                                    f'{self.task_config.APREFIX}ocn.{string}.nc')])

        # Copy DA grid (computed for the start of the window)
        post_file_list.append([os.path.join(self.task_config.DATA,
                                            'soca_gridspec.nc'),
                               os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                            f'{self.task_config.RUN}.t{bcyc}z.ocngrid.nc')])

        # Copy the CICE analysis restart
        if os.getenv('DOIAU') == "YES":
            cice_rst_date = self.task_config.MARINE_WINDOW_BEGIN.strftime('%Y%m%d.%H%M%S')
        else:
            cice_rst_date = self.task_config.CDATE.strftime('%Y%m%d.%H%M%S')

        post_file_list.append([os.path.join(self.task_config.DATA, 'Data',
                                            f'{cice_rst_date}.cice_model.res.nc'),
                               os.path.join(self.task_config.COMOUT_ICE_ANALYSIS,
                                            f'{cice_rst_date}.cice_model_anl.res.nc')])

        FileHandler({'copy': post_file_list}).sync()

        # create COM sub-directories
        FileHandler({'mkdir': [os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS, 'diags'),
                               os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS, 'bump'),
                               os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS, 'yaml')]}).sync()

        # ioda output files
        fh_list = list_all_files(os.path.join(self.task_config.DATA, 'diags'),
                                 os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS, 'diags'))

        # yaml configurations
        fh_list = list_all_files(os.path.join(self.task_config.DATA),
                                 os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS, 'yaml'), wc='*.yaml', fh_list=fh_list)

        FileHandler({'copy': fh_list}).sync()

        # obs space statistics
        fh_list = list_all_files(os.path.join(self.task_config.DATA),
                                 os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS), wc='*stats.csv', fh_list=fh_list)
        FileHandler({'copy': fh_list}).sync()

    @logit(logger)
    def obs_space_stats(self: Task) -> None:
        """Observation space statistics
           This method computes a few basic statistics on the observation spaces
        """

        # obs space statistics
        logger.info(f"---------------- Compute basic stats")
        diags_list = glob.glob(os.path.join(os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS, 'diags', '*.nc4')))
        obsstats_j2yaml = str(os.path.join(self.task_config.PARMgfs, 'gdas', 'soca', 'obs', 'obs_stats.yaml.j2'))

        # function to create a minimalist ioda obs sapce
        def create_obs_space(data):
            os_dict = {"obs space": {
                       "name": data["obs_space"],
                       "obsdatain": {
                           "engine": {"type": "H5File", "obsfile": data["obsfile"]}
                       },
                       "simulated variables": [data["variable"]]
                       },
                       "variable": data["variable"],
                       "experiment identifier": data["pslot"],
                       "csv output": data["csv_output"]
                       }
            return os_dict

        # get the experiment id
        pslot = self.task_config.PSLOT

        # iterate through the obs spaces and generate the yaml for gdassoca_obsstats.x
        obs_spaces = []
        for obsfile in diags_list:

            # define an obs space name
            obs_space = re.sub(r'\.\d{10}\.nc4$', '', os.path.basename(obsfile))

            # get the variable name, assume 1 variable per file
            nc = netCDF4.Dataset(obsfile, 'r')
            variable = next(iter(nc.groups["ombg"].variables))
            nc.close()

            # filling values for the templated yaml
            data = {'obs_space': os.path.basename(obsfile),
                    'obsfile': obsfile,
                    'pslot': pslot,
                    'variable': variable,
                    'csv_output': os.path.join(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                               f"{self.task_config.OPREFIX}ocn.{obs_space}.stats.csv")}
            obs_spaces.append(create_obs_space(data))

        # create the yaml
        data = {'obs_spaces': obs_spaces}
        conf = parse_j2yaml(path=obsstats_j2yaml, data=data)
        stats_yaml = 'diag_stats.yaml'
        conf.save(stats_yaml)

        # run the application
        mdau.link_executable(self.task_config, 'gdassoca_obsstats.x')
        command = f"{os.getenv('launcher')} -n 1"
        exec_cmd = Executable(command)
        exec_name = os.path.join(self.task_config.DATA, 'gdassoca_obsstats.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg(stats_yaml)

        mdau.run(exec_cmd)
