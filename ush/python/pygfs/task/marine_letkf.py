#!/usr/bin/env python3

from datetime import timedelta
import f90nml
from glob import glob 
from logging import getLogger
from os import path
from pygfs.task.analysis import Analysis
from soca import bkg_utils
from typing import Dict
import ufsda
from wxflow import (Executable,
                    FileHandler,
                    logit,
                    parse_j2yaml,
                    WorkflowException,
                    YAMLFile)

logger = getLogger(__name__.split('.')[-1])


class MarineLETKF(Analysis):
    """
    Class for global ocean and sea ice analysis LETKF task
    """

    @logit(logger, name="MarineLETKF")
    def __init__(self, config: Dict) -> None:
        """Constructor for ocean and sea ice LETKF task
        Parameters:
        ------------
        config: Dict
            configuration, namely evironment variables
        Returns:
        --------
        None
        """

        logger.info("init")
        super().__init__(config)

        PDY = self.runtime_config.PDY
        cyc = self.runtime_config.cyc
        gcyc = str(self.config.gcyc).zfill(2)
        self.runtime_config.gcyc = gcyc
        RUN = self.runtime_config.RUN
        DATA = path.realpath(self.runtime_config.DATA)
        cdate = PDY + timedelta(hours=cyc)
        COM_TOP_PREV_ENS = self.config.COM_TOP_PREV_ENS

        gdas_home = path.join(config['HOMEgfs'], 'sorc', 'gdas.cd')
        exec_dir = path.join(gdas_home, 'build', 'bin')
    
        half_assim_freq = timedelta(hours=int(config['assim_freq'])/2)
        window_begin = cdate - half_assim_freq
        window_begin_iso = window_begin.strftime('%Y-%m-%dT%H:%M:%SZ')
        window_middle_iso = cdate.strftime('%Y-%m-%dT%H:%M:%SZ')
        self.config.ATM_WINDOW_BEGIN = window_begin_iso
        self.config.ATM_WINDOW_MIDDLE = window_middle_iso

        self.config.letkf_exec = path.join(exec_dir, 'gdas.x')
        letkf_yaml_dir = path.join(gdas_home, 'parm', 'soca', 'letkf')
        self.config['letkf_yaml_template'] = path.join(letkf_yaml_dir, 'letkf.yaml.j2')
        letkf_yaml_file = path.join(DATA, 'letkf.yaml')
        self.config.letkf_exec_args =  f"fv3jedi localensembleda {letkf_yaml_file}"
        self.config.letkf_yaml_file = letkf_yaml_file

        self.config['window_begin'] = window_begin
        self.config['BKG_LIST'] = 'bkg_list.yaml'
        self.config['bkg_dir'] = path.join(DATA, 'bkg')

        self.config.exec_name_gridgen = path.join(self.config.JEDI_BIN, 'gdas_soca_gridgen.x')
        self.config['gridgen_yaml'] = path.join(gdas_home, 'parm', 'soca', 'gridgen', 'gridgen.yaml')

        self.config.mom_input_nml_src = path.join(gdas_home, 'parm', 'soca', 'fms', 'input.nml')
        self.config.mom_input_nml_tmpl = path.join(DATA, 'mom_input.nml.tmpl')
        self.config.mom_input_nml = path.join(DATA, 'mom_input.nml')

        self.config.data_output_dir = path.join(DATA, 'data_output')
        self.config.ens_dir = path.join(DATA, 'ens')
        self.config.obs_dir = path.join(DATA, 'obs')

        # set up lists of files for ens background
        ocn_ens_bkg_filename = f"enkf{RUN}.ocean.t{gcyc}z.inst.f009.nc"
        ice_ens_bkg_filename = f"enkf{RUN}.ice.t{gcyc}z.inst.f009.nc"

        # create list of subdirs to make in initialize, and list of some of the files to stage
        ens_bkg_files_to_stage = []
        dirs_to_make = [self.config.bkg_dir, self.config.data_output_dir, self.config.obs_dir]
        for mem in range(1,self.config.NMEM_ENS+1):
            mem_dir = f'mem{str(mem).zfill(3)}' # will make pattern mem001
            dirs_to_make.append(path.join(self.config.ens_dir,mem_dir))
            ocn_file_path = path.join(COM_TOP_PREV_ENS,
                                      mem_dir,
                                      'model_data',
                                      'ocean',
                                      'history',
                                      ocn_ens_bkg_filename)
            ocn_file_dest = path.join(self.config.ens_dir,
                                      mem_dir,
                                      ocn_ens_bkg_filename)
            ice_file_path = path.join(COM_TOP_PREV_ENS,
                                      mem_dir,
                                      'model_data',
                                      'ice',
                                      'history',
                                      ice_ens_bkg_filename)
            ice_file_dest = path.join(self.config.ens_dir,
                                      mem_dir,
                                      ice_ens_bkg_filename)
            ens_bkg_files_to_stage.append((ocn_file_path, ocn_file_dest))
            ens_bkg_files_to_stage.append((ice_file_path, ice_file_dest))

        self.config.ens_bkg_files_to_stage = ens_bkg_files_to_stage
        self.config.dirs_to_make = dirs_to_make

    @logit(logger)
    def initialize(self):
        """Method initialize for ocean and sea ice LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("initialize")

        cyc = self.runtime_config.cyc
        DATA = self.runtime_config.DATA
        ens_dir = self.config.ens_dir
        PDYstr = self.runtime_config.PDY.strftime("%Y%m%d")
        RUN = self.runtime_config.RUN

        # create directories under DATA
        FileHandler({'mkdir': self.config.dirs_to_make}).sync()

        # copy ensemble background to DATA/ens/mem???
        FileHandler({'copy': self.config.ens_bkg_files_to_stage }).sync()

        bkg_utils.gen_bkg_list(bkg_path=self.config.COM_OCEAN_HISTORY_PREV,
                               out_path=self.config.bkg_dir,
                               window_begin=self.config.window_begin,
                               yaml_name=self.config.BKG_LIST)

        # TODO(AFE): probably needs to be jinjafied
        obs_list = YAMLFile(self.config.OBS_YAML)

        # get the list of observations
        obs_files = []
        for ob in obs_list['observers']:
            obs_name = ob['obs space']['name'].lower()
            obs_filename = f"{RUN}.t{cyc}z.{obs_name}.{PDYstr}{cyc}.nc4"
            obs_files.append((obs_filename,ob))

        obs_files_to_copy = []
        obs_to_use = []
        # copy obs from COM_OBS to DATA/obs
        for obs_file, ob in obs_files:
            logger.info(f"******* {obs_file}")
            obs_src = path.join(self.config.COM_OBS, obs_file)
            obs_dst = path.join(DATA, self.config.obs_dir, obs_file)
            logger.info(f"******* {obs_src}")
            if path.exists(obs_src):
                logger.info(f"******* fetching {obs_file}")
                obs_files_to_copy.append([obs_src, obs_dst])
                obs_to_use.append(ob)
            else:
                logger.info(f"******* {obs_file} is not in the database")

        FileHandler({'copy': obs_files_to_copy}).sync()

        # make the letkf.yaml
        letkf_yaml = parse_j2yaml(self.config.letkf_yaml_template, self.config)
        letkf_yaml.observations.observers = obs_to_use
        letkf_yaml.save(self.config.letkf_yaml_file)

        FileHandler({'copy': [[self.config.mom_input_nml_src,
            self.config.mom_input_nml_tmpl]]}).sync()
       
        self.stage_fix_files()

        bkg_utils.stage_ic(self.config.bkg_dir, self.runtime_config.DATA, self.runtime_config.gcyc)

        # swap date and stack size
        domain_stack_size = self.config.DOMAIN_STACK_SIZE
        ymdhms = [int(s) for s in self.config.window_begin.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
        with open(self.config.mom_input_nml_tmpl, 'r') as nml_file:
            nml = f90nml.read(nml_file)
            nml['ocean_solo_nml']['date_init'] = ymdhms
            nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
            ufsda.disk_utils.removefile(self.config.mom_input_nml)
            nml.write(self.config.mom_input_nml)
        

    @logit(logger)
    def run(self):
        """Method run for ocean and sea ice LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("run")

        exec_cmd_gridgen = Executable(self.config.APRUN_OCNANALLETKF)
        exec_cmd_gridgen.add_default_arg(self.config.exec_name_gridgen)
        exec_cmd_gridgen.add_default_arg(self.config.gridgen_yaml)

        try:
            logger.debug(f"Executing {exec_cmd_gridgen}")
            exec_cmd_gridgen()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd_gridgen}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd_gridgen}")
        pass

        exec_cmd_letkf = Executable(self.config.APRUN_OCNANALLETKF)
        exec_cmd_letkf.add_default_arg(self.config.letkf_exec)
#        exec_cmd_letkf.add_default_arg(self.config.letkf_exec_args )
        exec_cmd_letkf.add_default_arg('fv3jedi') 
        exec_cmd_letkf.add_default_arg('localensembleda')
        exec_cmd_letkf.add_default_arg(self.config.letkf_yaml_file )

        try:
            logger.debug(f"Executing {exec_cmd_letkf}")
            exec_cmd_letkf()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd_letkf}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd_letkf}")
        pass


    @logit(logger)
    def finalize(self):
        """Method finalize for ocean and sea ice LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("finalize")

    @logit(logger)
    def stage_fix_files(self):
        """Stage fixed files for marine DA
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """
        # adapted from ufsda stage_fix
        #TODO(AFE): this method maybe should go in a different class

        logger.info("stage_fix_files")

        DATA = self.runtime_config.DATA
        SOCA_INPUT_FIX_DIR = self.config.SOCA_INPUT_FIX_DIR

        fix_files = []
        # copy Rossby Radius file
        fix_files.append([path.join(SOCA_INPUT_FIX_DIR, 'rossrad.dat'),
                          path.join(DATA, 'rossrad.dat')])
        # link name lists
        fix_files.append([path.join(SOCA_INPUT_FIX_DIR, 'field_table'),
                          path.join(DATA, 'field_table')])
        fix_files.append([path.join(SOCA_INPUT_FIX_DIR, 'diag_table'),
                          path.join(DATA, 'diag_table')])
        fix_files.append([path.join(SOCA_INPUT_FIX_DIR, 'MOM_input'),
                          path.join(DATA, 'MOM_input')])
        # link field metadata
        fix_files.append([path.join(SOCA_INPUT_FIX_DIR, 'fields_metadata.yaml'),
                          path.join(DATA, 'fields_metadata.yaml')])
    
        # link ufo <---> soca name variable mapping
        fix_files.append([path.join(SOCA_INPUT_FIX_DIR, 'obsop_name_map.yaml'),
                          path.join(DATA, 'obsop_name_map.yaml')])
    
        # INPUT
        src_input_dir = path.join(SOCA_INPUT_FIX_DIR, 'INPUT')
        dst_input_dir = path.join(DATA, 'INPUT')
        FileHandler({'mkdir': [dst_input_dir]}).sync()
    
        input_files = glob(f'{src_input_dir}/*')
        for input_file in input_files:
            fname = path.basename(input_file)
            fix_files.append([path.join(src_input_dir, fname),
                              path.join(dst_input_dir, fname)])
    
        FileHandler({'copy': fix_files}).sync()