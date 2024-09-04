#!/usr/bin/env python3

import copy
import os
import glob
from logging import getLogger
import pygfs.utils.marine_da_utils as mdau
import re
import yaml
from jcb import render

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta, to_YMD,
                    parse_j2yaml,
                    logit,
                    Executable,
                    Task,
                    Template, TemplateConstants, YAMLFile)

logger = getLogger(__name__.split('.')[-1])


def parse_obs_list_file(gdas_home):
    # Get the list of observation types from the obs_list.yaml
    obs_list_path = os.path.join(gdas_home, 'parm', 'soca', 'obs', 'obs_list.yaml')
    obs_types = []
    with open(obs_list_path, 'r') as file:
        for line in file:
            # Remove leading/trailing whitespace and check if the line is uncommented
            line = line.strip()
            if line.startswith('- !INC') and not line.startswith('#'):
                # Extract the type using regex
                match = re.search(r'\$\{OBS_YAML_DIR\}/(.+)\.yaml', line)
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
        _home_gdas = os.path.join(self.task_config.HOMEgfs, 'sorc', 'gdas.cd')
        _calc_scale_exec = os.path.join(self.task_config.HOMEgfs, 'ush', 'soca', 'calc_scales.py')
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_begin_iso = _window_begin.strftime('%Y-%m-%dT%H:%M:%SZ')
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        if self.task_config.NMEM_ENS > 0:
            _enspert_relpath = os.path.relpath(self.task_config.DATAenspert, self.task_config.DATA)
        else:
            _enspert_relpath = None

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'HOMEgdas': _home_gdas,
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_BEGIN_ISO': _window_begin_iso,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'BERROR_YAML_DIR': os.path.join(_home_gdas, 'parm', 'soca', 'berror'),
                'UTILITY_YAML_TMPL': os.path.join(_home_gdas, 'parm', 'soca', 'soca_utils_stage.yaml.j2'),
                'MARINE_ENSDA_STAGE_BKG_YAML_TMPL': os.path.join(_home_gdas, 'parm', 'soca', 'ensda', 'stage_ens_mem.yaml.j2'),
                'MARINE_DET_STAGE_BKG_YAML_TMPL': os.path.join(_home_gdas, 'parm', 'soca', 'soca_det_bkg_stage.yaml.j2'),
                'MARINE_OBS_LIST_YAML': os.path.join(_home_gdas, 'parm', 'soca', 'obs', 'obs_list.yaml'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z."
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

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
        # TODO(GV or AE): Keep a copy of the obs in the scratch fs after the obs prep job
        self._fetch_observations()

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
        logger.info(f"Staging SOCA utility yaml files from {self.task_config.HOMEgfs}/parm/gdas/soca")
        soca_utility_list = parse_j2yaml(self.task_config.UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

        # stage the soca grid
        FileHandler({'copy': [[os.path.join(self.task_config.COMIN_OCEAN_BMATRIX, 'soca_gridspec.nc'),
                               os.path.join(self.task_config.DATA, 'soca_gridspec.nc')]]}).sync()

        # link the static B resources
        os.symlink('../staticb', 'staticb')

        # hybrid EnVAR case
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            # stage ensemble membersfiles for use in hybrid background error
            logger.debug(f"Stage ensemble members for the hybrid background error")
            mdau.stage_ens_mem(self.task_config)

        # prepare the yaml configuration to run the SOCA variational application
        self._prep_variational_yaml()

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
            logger.info(f"******** {self.task_config.APREFIX}{ob['obs space']['name'].lower()}.{to_YMD(self.task_config.PDY)}{self.task_config.cyc}.nc4")
            obs_files.append(f"{self.task_config.APREFIX}{ob['obs space']['name'].lower()}.{to_YMD(self.task_config.PDY)}{self.task_config.cyc}.nc4")
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
        """Create the necesssary directory structure to run the SOCA variational application
        """
        logger.info(f"---------------- Setup runtime environement")

        anl_dir = self.task_config.DATA

        # create analysis directories
        diags = os.path.join(anl_dir, 'diags')            # output dir for soca DA obs space
        obs_in = os.path.join(anl_dir, 'obs')             # input      "           "
        anl_out = os.path.join(anl_dir, 'Data')           # output dir for soca DA
        FileHandler({'mkdir': [diags, obs_in, anl_out]}).sync()

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
        envconfig_jcb['nmem_ens'] = self.task_config.NMEM_ENS
        envconfig_jcb['berror_model'] = 'marine_background_error_static_diffusion'
        if self.task_config.NMEM_ENS > 3:
            envconfig_jcb['berror_model'] = 'marine_background_error_hybrid_diffusion_diffusion'
        envconfig_jcb['DATA'] = self.task_config.DATA
        envconfig_jcb['OPREFIX'] = self.task_config.OPREFIX
        envconfig_jcb['PDY'] = os.getenv('PDY')
        envconfig_jcb['cyc'] = os.getenv('cyc')
        envconfig_jcb['SOCA_NINNER'] = self.task_config.SOCA_NINNER
        envconfig_jcb['obs_list'] = ['adt_rads_all']

        # Write obs_list_short
        with open('obs_list_short.yaml', 'w') as file:
            yaml.dump(parse_obs_list_file(self.task_config.HOMEgdas), file, default_flow_style=False)
        os.environ['OBS_LIST_SHORT'] = 'obs_list_short.yaml'

        # Render the JCB configuration files
        jcb_base_yaml = os.path.join(self.task_config.HOMEgdas, 'parm', 'soca', 'marine-jcb-base.yaml')
        jcb_algo_yaml = os.path.join(self.task_config.HOMEgdas, 'parm', 'soca', 'marine-jcb-3dfgat.yaml.j2')

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

        # Render the full JEDI configuration file using JCB
        jedi_config = render(jcb_config)

        # Save the JEDI configuration file
        var_yaml_jcb = 'var.yaml'
        mdau.clean_empty_obsspaces(jedi_config, target=var_yaml_jcb, app='var')


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
    def finalize(self: Task) -> None:
        """Finalize the marine analysis job

        This method will finalize the marine analysis job.
        This includes:
        -
        - ...

        """
