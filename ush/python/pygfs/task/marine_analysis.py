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

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta, to_YMD,
                    parse_j2yaml, parse_yaml,
                    logit,
                    Executable,
                    Task,
                    save_as_yaml,
                    Template, TemplateConstants, YAMLFile)

logger = getLogger(__name__.split('.')[-1])


def parse_obs_list_file(obs_list_yaml_path):
    # Get the list of observation types from the obs_list.yaml
    obs_types = []
    with open(obs_list_yaml_path, 'r') as file:
        for line in file:
            # Remove leading/trailing whitespace and check if the line is uncommented
            line = line.strip()
            if line.startswith('- !INC') and not line.startswith('#'):
                # Extract the type using regex
                match = re.search(r'\$\{MARINE_OBS_YAML_DIR\}/(.+)\.yaml', line)
                if match:
                    obs_types.append(str(match.group(1)))
    return obs_types


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

        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        if self.task_config.NMEM_ENS > 0:
            _enspert_relpath = os.path.relpath(self.task_config.DATAenspert, self.task_config.DATA)
        else:
            _enspert_relpath = None

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'PARMsoca': os.path.join(self.task_config.PARMgfs, 'gdas', 'soca'),
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_BEGIN_ISO': _window_begin.strftime('%Y-%m-%dT%H:%M:%SZ'),
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'MARINE_WINDOW_MIDDLE_ISO': self.task_config.current_cycle.strftime('%Y-%m-%dT%H:%M:%SZ'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z."
            }
        )

        # Extend task_config with local_dict
        self.task_config.update(local_dict)

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
        super().initialize()

        # prepare the directory structure to run SOCA
        self._prep_scratch_dir()

        # fetch observations from COMROOT
        # TODO(G.V. or A.E.): Keep a copy of the obs in the scratch fs after the obs prep job
        self._fetch_observations()

        # stage the ocean and ice backgrounds for FGAT
        bkg_list = parse_j2yaml(self.task_config.MARINE_DET_STAGE_BKG_YAML_TMPL, self.task_config)
        FileHandler(bkg_list).sync()

        # stage the soca grid
        FileHandler({'copy': [[os.path.join(self.task_config.COMIN_OCEAN_BMATRIX, 'soca_gridspec.nc'),
                               os.path.join(self.task_config.DATA, 'soca_gridspec.nc')]]}).sync()

        # link the flow dependent static B resources from the B-matrix task of the same cycle
        os.symlink('../staticb', 'staticb')

        # hybrid EnVAR case
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            # stage ensemble membersfiles for use in hybrid background error
            logger.debug(f"Stage ensemble members for the hybrid background error")
            mdau.stage_ens_mem(self.task_config)

        # prepare the yaml configuration to run the SOCA variational application
        self._prep_variational_yaml()

        # prepare the yaml configuration to run the SOCA to MOM6 IAU increment
        self._prep_checkpoint()

    @logit(logger)
    def _fetch_observations(self: Task) -> None:
        """Fetch observations from COMIN_OBS

        This method will fetch the observations for the cycle and check the
        list against what is available for the cycle.
        """

        # get the list of observations
        obs_list_config = YAMLFile(self.task_config.MARINE_OBS_LIST_YAML)
        obs_list_config = Template.substitute_structure(obs_list_config, TemplateConstants.DOLLAR_PARENTHESES, self.task_config)
        obs_list_config = {'observations': obs_list_config}
        logger.info(f"{obs_list_config}")

        obs_files = []
        for ob in obs_list_config['observations']['observers']:
            logger.info(f"******** {self.task_config.OPREFIX}{ob['obs space']['name'].lower()}.{to_YMD(self.task_config.PDY)}{self.task_config.cyc}.nc4")
            obs_files.append(f"{self.task_config.OPREFIX}{ob['obs space']['name'].lower()}.{to_YMD(self.task_config.PDY)}{self.task_config.cyc}.nc4")
        obs_list = []

        # copy obs from COM_OBS to DATA/obs
        for obs_file in obs_files:
            logger.info(f"******* {obs_file}")
            obs_src = os.path.join(self.task_config.COM_OBS, obs_file)
            obs_dst = os.path.join(self.task_config.DATA, 'obs', obs_file)
            logger.info(f"******* {obs_src}")
            if os.path.exists(obs_src):
                logger.info(f"******* fetching {obs_file}")
                obs_list.append([obs_src, obs_dst])
            else:
                logger.info(f"******* {obs_file} is not in the database")

        FileHandler({'copy': obs_list}).sync()

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

        # prepare the MOM6 input.nml
        mdau.prep_input_nml(self.task_config)

        # stage the soca utility yamls (gridgen, fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files from {self.task_config.PARMsoca}")
        soca_utility_list = parse_j2yaml(self.task_config.MARINE_UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

    @logit(logger)
    def _prep_variational_yaml(self: Task) -> None:
        """Create the yaml configuration to run the SOCA variational application
        """

        # prepare background list for the pseudo model, check bkg date for consistency
        mdau.gen_bkg_list(bkg_path='./bkg',
                          window_begin=self.task_config.MARINE_WINDOW_BEGIN,
                          yaml_name='bkg_list.yaml')

        # Make a copy of the env config before modifying to avoid breaking something else
        envconfig_jcb = copy.deepcopy(self.task_config)
        logger.info(f"---------------- Prepare the yaml configuration")
        logger.info(f"{envconfig_jcb}")       # Prepare the yaml configuration

        # Add the things to the envconfig in order to template JCB files
        envconfig_jcb['PARMgfs'] = self.task_config.PARMgfs
        envconfig_jcb['NMEM_ENS'] = self.task_config.NMEM_ENS
        envconfig_jcb['berror_model'] = 'marine_background_error_static_diffusion'
        if self.task_config.NMEM_ENS > 3:
            envconfig_jcb['berror_model'] = 'marine_background_error_hybrid_diffusion_diffusion'
        envconfig_jcb['DATA'] = self.task_config.DATA
        envconfig_jcb['OPREFIX'] = self.task_config.OPREFIX
        envconfig_jcb['PDY'] = os.getenv('PDY')
        envconfig_jcb['cyc'] = os.getenv('cyc')
        envconfig_jcb['SOCA_NINNER'] = self.task_config.SOCA_NINNER
        envconfig_jcb['obs_list'] = ['adt_rads_all']
        envconfig_jcb['MOM6_LEVS'] = mdau.get_mom6_levels(str(self.task_config.OCNRES))

        # Write obs_list_short
        save_as_yaml(parse_obs_list_file(self.task_config.MARINE_OBS_LIST_YAML), 'obs_list_short.yaml')
        os.environ['OBS_LIST_SHORT'] = 'obs_list_short.yaml'

        # Render the JCB configuration files
        jcb_base_yaml = os.path.join(self.task_config.PARMsoca, 'marine-jcb-base.yaml')
        jcb_algo_yaml = os.path.join(self.task_config.PARMsoca, 'marine-jcb-3dfgat.yaml.j2')

        jcb_base_config = YAMLFile(path=jcb_base_yaml)
        jcb_base_config = Template.substitute_structure(jcb_base_config, TemplateConstants.DOUBLE_CURLY_BRACES, envconfig_jcb.get)
        jcb_base_config = Template.substitute_structure(jcb_base_config, TemplateConstants.DOLLAR_PARENTHESES, envconfig_jcb.get)
        jcb_algo_config = YAMLFile(path=jcb_algo_yaml)
        jcb_algo_config = Template.substitute_structure(jcb_algo_config, TemplateConstants.DOUBLE_CURLY_BRACES, envconfig_jcb.get)
        jcb_algo_config = Template.substitute_structure(jcb_algo_config, TemplateConstants.DOLLAR_PARENTHESES, envconfig_jcb.get)

        # Override base with the application specific config
        jcb_config = {**jcb_base_config, **jcb_algo_config}

        # convert datetime to string
        jcb_config['window_begin'] = self.task_config.MARINE_WINDOW_BEGIN.strftime('%Y-%m-%dT%H:%M:%SZ')
        jcb_config['window_middle'] = self.task_config.MARINE_WINDOW_MIDDLE.strftime('%Y-%m-%dT%H:%M:%SZ')

        # Current hack so that this is not done directly in the JCB base yaml
        jcb_config['marine_pseudo_model_states'] = parse_yaml('bkg_list.yaml')

        # Render the full JEDI configuration file using JCB
        jedi_config = render(jcb_config)

        # Save the JEDI configuration file
        var_yaml_jcb = 'var.yaml'
        mdau.clean_empty_obsspaces(jedi_config, target=var_yaml_jcb, app='var')

    def _prep_checkpoint(self: Task) -> None:
        """Create the yaml configuration to run the SOCA to MOM6 IAU increment
        """
        # prepare the socaincr2mom6.yaml
        logger.info("Generate the SOCA to MOM6 IAU increment YAML file")
        data = {'marine_window_begin': self.task_config.MARINE_WINDOW_BEGIN_ISO,
                'marine_window_middle': self.task_config.MARINE_WINDOW_MIDDLE_ISO}
        soca2mom6inc_config = parse_j2yaml(path=os.path.join(self.task_config.MARINE_JCB_GDAS_ALGO, 'socaincr2mom6.yaml.j2'),
                                           data=data)
        soca2mom6inc_config.save(os.path.join(self.task_config.DATA, 'socaincr2mom6.yaml'))

        # prepare the SOCA to CICE YAML file
        logger.info("Generate the SOCA to CICE RST YAML file")

        # set the restart date, dependent on the cycling type
        if self.task_config.DOIAU:
            # forecast initialized at the begining of the DA window
            fcst_begin = self.task_config.MARINE_WINDOW_BEGIN_ISO
            rst_date = self.task_config.MARINE_WINDOW_BEGIN.strftime('%Y%m%d.%H%M%S')
        else:
            # forecast initialized at the middle of the DA window
            fcst_begin = self.task_config.MARINE_WINDOW_MIDDLE_ISO
            rst_date = self.task_config.MARINE_WINDOW_MIDDLE.strftime('%Y%m%d.%H%M%S')

        # make a copy of the CICE6 restart
        ice_rst = os.path.join(self.task_config.COMIN_ICE_RESTART_PREV, f'{rst_date}.cice_model.res.nc')
        ice_rst_ana = os.path.join(self.task_config.DATA, 'Data', rst_date + '.cice_model.res.nc')
        FileHandler({'copy': [[ice_rst, ice_rst_ana]]}).sync()

        # prepare the necessary configuration for the SOCA to CICE application
        soca2cice_param = AttrDict({
            "ocn_ana": f"./Data/ocn.3dvarfgat_pseudo.an.{self.task_config.MARINE_WINDOW_MIDDLE_ISO}.nc",
            "ice_ana": f"./Data/ice.3dvarfgat_pseudo.an.{self.task_config.MARINE_WINDOW_MIDDLE_ISO}.nc",
            "ice_rst": ice_rst_ana,
            "fcst_begin": fcst_begin
        })
        logger.debug(f"{soca2cice_param}")

        # render the SOCA to CICE YAML file for the Arctic and Antarctic
        logger.info("render the SOCA to CICE YAML file for the Arctic and Antarctic")
        varchgyamls = ['soca_2cice_arctic.yaml', 'soca_2cice_antarctic.yaml']
        for varchgyaml in varchgyamls:
            soca2cice_config = parse_j2yaml(path=os.path.join(self.task_config.MARINE_JCB_GDAS_ALGO, f'{varchgyaml}.j2'),
                                            data=soca2cice_param)
            soca2cice_config.save(os.path.join(self.task_config.DATA, varchgyaml))

    @logit(logger)
    def variational(self: Task) -> None:
        # link gdas_soca_gridgen.x
        mdau.link_executable(self.task_config, 'gdas.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEANLVAR)
        exec_name = os.path.join(self.task_config.DATA, 'gdas.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca')
        exec_cmd.add_default_arg('variational')
        exec_cmd.add_default_arg('var.yaml')

        mdau.run(exec_cmd)

    @logit(logger)
    def checkpoint_cice6(self: Task, soca2ciceyaml) -> None:
        # link gdas_soca_gridgen.x
        mdau.link_executable(self.task_config, 'gdas.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEANLCHKPT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca')
        exec_cmd.add_default_arg('convertstate')
        exec_cmd.add_default_arg(soca2ciceyaml)

        mdau.run(exec_cmd)

    @logit(logger)
    def checkpoint_mom6_iau(self: Task, socaincr2mom6yaml) -> None:
        # link gdas_incr_handler.x
        mdau.link_executable(self.task_config, 'gdas_incr_handler.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEANLCHKPT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_incr_handler.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg(socaincr2mom6yaml)

        mdau.run(exec_cmd)

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
        com_ocean_analysis = self.task_config.COMOUT_OCEAN_ANALYSIS
        com_ice_analysis = self.task_config.COMOUT_ICE_ANALYSIS
        com_ice_restart = self.task_config.COMOUT_ICE_RESTART
        anl_dir = self.task_config.DATA
        cdate = self.task_config.CDATE
        pdy = self.task_config.PDY
        staticsoca_dir = self.task_config.SOCA_INPUT_FIX_DIR
        RUN = self.task_config.RUN
        cyc = str(self.task_config.cyc).zfill(2)
        bcyc = str(self.task_config.MARINE_WINDOW_BEGIN.hour).zfill(2)
        bdate = self.task_config.MARINE_WINDOW_BEGIN_ISO
        mdate = self.task_config.MARINE_WINDOW_MIDDLE_ISO
        nmem_ens = int(self.task_config.NMEM_ENS)

        logger.info(f"---------------- Copy from RUNDIR to COMOUT")

        post_file_list = []

        # Make a copy the IAU increment
        post_file_list.append([os.path.join(anl_dir, 'inc.nc'),
                               os.path.join(com_ocean_analysis, f'{RUN}.t{cyc}z.ocninc.nc')])

        domains = ['ocn', 'ice']
        for domain in domains:
            '''
            # Copy of the diagonal of the background error for the cycle
            post_file_list.append([os.path.join(anl_dir, f'{domain}.bkgerr_stddev.incr.{mdate}.nc'),
                                   os.path.join(com_ocean_analysis, f'{RUN}.t{cyc}z.{domain}.bkgerr_stddev.nc')])

            # Copy the recentering error
            if nmem_ens > 2:
                post_file_list.append([os.path.join(anl_dir, 'static_ens', f'{domain}.ssh_recentering_error.incr.{bdate}.nc'),
                                       os.path.join(com_ocean_analysis, f'{RUN}.t{cyc}z.{domain}.recentering_error.nc')])
            '''

            # Copy the ice and ocean increments
            post_file_list.append([os.path.join(anl_dir, 'Data', f'{domain}.3dvarfgat_pseudo.incr.{mdate}.nc'),
                                   os.path.join(com_ocean_analysis, f'{RUN}.t{cyc}z.{domain}.incr.nc')])

            # Copy the analysis at the start of the window
            post_file_list.append([os.path.join(anl_dir, 'Data', f'{domain}.3dvarfgat_pseudo.an.{mdate}.nc'),
                                   os.path.join(com_ocean_analysis, f'{RUN}.t{cyc}z.{domain}ana.nc')])

        # Copy of the ssh diagnostics
        '''
        if nmem_ens > 2:
            for string in ['ssh_steric_stddev', 'ssh_unbal_stddev', 'ssh_total_stddev', 'steric_explained_variance']:
                post_file_list.append([os.path.join(anl_dir, 'static_ens', f'ocn.{string}.incr.{bdate}.nc'),
                                       os.path.join(com_ocean_analysis, f'{RUN}.t{cyc}z.ocn.{string}.nc')])
        '''

        # Copy DA grid (computed for the start of the window)
        post_file_list.append([os.path.join(anl_dir, 'soca_gridspec.nc'),
                               os.path.join(com_ocean_analysis, f'{RUN}.t{bcyc}z.ocngrid.nc')])

        # Copy the CICE analysis restart
        if os.getenv('DOIAU') == "YES":
            cice_rst_date = self.task_config.MARINE_WINDOW_BEGIN.strftime('%Y%m%d.%H%M%S')
        else:
            cice_rst_date = cdate.strftime('%Y%m%d.%H%M%S')

        post_file_list.append([os.path.join(anl_dir, 'Data', f'{cice_rst_date}.cice_model.res.nc'),
                               os.path.join(com_ice_analysis, f'{cice_rst_date}.cice_model_anl.res.nc')])

        FileHandler({'copy': post_file_list}).sync()

        # create COM sub-directories
        FileHandler({'mkdir': [os.path.join(com_ocean_analysis, 'diags'),
                               os.path.join(com_ocean_analysis, 'bump'),
                               os.path.join(com_ocean_analysis, 'yaml')]}).sync()

        # ioda output files
        fh_list = list_all_files(os.path.join(anl_dir, 'diags'),
                                 os.path.join(com_ocean_analysis, 'diags'))

        # yaml configurations
        fh_list = list_all_files(os.path.join(anl_dir),
                                 os.path.join(com_ocean_analysis, 'yaml'), wc='*.yaml', fh_list=fh_list)

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
            variable = next(iter(nc.groups["ObsValue"].variables))
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
