#!/usr/bin/env python3

from datetime import timedelta
import f90nml
from logging import getLogger
from os import path
from pygfs.task.analysis import Analysis
from soca import bkg_utils
from typing import Dict
import ufsda
from ufsda.stage import soca_fix
from wxflow import (AttrDict,
                    chdir,
                    Executable,
                    FileHandler,
                    logit,
                    parse_j2yaml,
                    Task,
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

        PDY = self.runtime_config['PDY']
        cyc = self.runtime_config['cyc']
        DATA = self.runtime_config.DATA
        cdate = PDY + timedelta(hours=cyc)

        gdas_home = path.join(config['HOMEgfs'], 'sorc', 'gdas.cd')

        half_assim_freq = timedelta(hours=int(config['assim_freq'])/2)
        window_begin = cdate - half_assim_freq
        window_begin_iso = window_begin.strftime('%Y-%m-%dT%H:%M:%SZ')
        window_middle_iso = cdate.strftime('%Y-%m-%dT%H:%M:%SZ')
        letkf_yaml_dir = path.join(gdas_home, 'parm', 'soca', 'letkf')
        gdate = cdate - timedelta(hours=6)
        self.runtime_config['gcyc'] = gdate.strftime("%H")

        self.stage_config = AttrDict(
            {'window_begin': f"{window_begin.strftime('%Y-%m-%dT%H:%M:%SZ')}",
             'ATM_WINDOW_BEGIN': window_begin_iso,
             'ATM_WINDOW_MIDDLE': window_middle_iso,
             'DATA': DATA,
             'dump': self.runtime_config.RUN,
             'fv3jedi_stage_files': self.config.FV3JEDI_STAGE_YAML,
             'fv3jedi_stage': self.config.FV3JEDI_STAGE_YAML,
             'stage_dir': DATA,
             'soca_input_fix_dir': self.config.SOCA_INPUT_FIX_DIR,
             'NMEM_ENS': self.config.NMEM_ENS,
             'ATM_WINDOW_LENGTH': f"PT{config['assim_freq']}H"})

        self.config['letkf_yaml_template'] = path.join(letkf_yaml_dir, 'letkf.yaml.j2')
        self.config['letkf_yaml_file'] = path.join(DATA, 'letkf.yaml')

        self.config['window_begin'] = window_begin
        self.config['BKG_LIST'] = 'bkg_list.yaml'
        self.config['bkg_dir'] = path.join(DATA, 'bkg')

        self.config.exec_name_gridgen = path.join(self.config.JEDI_BIN, 'gdas_soca_gridgen.x')
        self.config['gridgen_yaml'] = path.join(gdas_home, 'parm', 'soca', 'gridgen', 'gridgen.yaml')

        self.config.mom_input_nml_src = path.join(gdas_home, 'parm', 'soca', 'fms', 'input.nml')
        self.config.mom_input_nml_tmpl = path.join(DATA, 'mom_input.nml.tmpl')
        self.config.mom_input_nml = path.join(DATA, 'mom_input.nml')

        self.config.data_output = 'data_output'
        self.config.ens_dir = 'ens'

        self.config.ATM_WINDOW_BEGIN = window_begin_iso
        self.config.ATM_WINDOW_MIDDLE = window_middle_iso
        
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


        PDYstr = self.runtime_config.PDY.strftime("%Y%m%d")

        FileHandler({'mkdir': [self.config.bkg_dir]}).sync()
        FileHandler({'mkdir': [self.config.data_output]}).sync()
        FileHandler({'mkdir': [self.config.ens_dir]}).sync()


        chdir(self.config.ens_dir)
        for mem in range(1,self.config.NMEM_ENS+1):
            #mem_dir = path.realpath(path.join(self.config.ens_dir , f'mem{str(mem).zfill(3)}'))
            #mem_dir = path.realpath(path.join('fdsfsds' , f'mem{str(mem).zfill(3)}'))
            mem_dir = f'mem{str(mem).zfill(3)}'
            print('mem_dir: ',mem_dir)
            FileHandler({'mkdir': [mem_dir]}).sync()
        chdir(self.runtime_config.DATA)
        bkg_utils.gen_bkg_list(bkg_path=self.config.COM_OCEAN_HISTORY_PREV,
                               out_path=self.config.bkg_dir,
                               window_begin=self.config.window_begin,
                               yaml_name=self.config.BKG_LIST)

        obs_list = YAMLFile(self.config.OBS_YAML)

        # get the list of observations
        obs_files = []
        for ob in obs_list['observers']:
            obs_name = ob['obs space']['name'].lower()
            obs_filename = f"{self.runtime_config.RUN}.t{self.runtime_config.cyc}z.{ob['obs space']['name'].lower()}.{PDYstr}{self.runtime_config.cyc}.nc4"
            obs_files.append((obs_filename,ob))
        obs_list = []

        obs_to_use = []
        # copy obs from COM_OBS to DATA/obs
        for obs_file, ob in obs_files:
            logger.info(f"******* {obs_file}")
            obs_src = path.join(self.config.COM_OBS, obs_file)
            obs_dst = path.join(self.runtime_config.DATA, obs_file)
            logger.info(f"******* {obs_src}")
            if path.exists(obs_src):
                logger.info(f"******* fetching {obs_file}")
                obs_list.append([obs_src, obs_dst])
                obs_to_use.append(ob)
            else:
                logger.info(f"******* {obs_file} is not in the database")

        FileHandler({'copy': obs_list}).sync()

        letkf_yaml = parse_j2yaml(self.config.letkf_yaml_template, self.config)
        letkf_yaml.observations.observers = obs_to_use
        letkf_yaml.save(self.config.letkf_yaml_file)

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

        chdir(self.runtime_config.DATA)

        FileHandler({'copy': [[self.config.mom_input_nml_src,
            self.config.mom_input_nml_tmpl]]}).sync()
       
        ufsda.stage.soca_fix(self.stage_config)

        bkg_utils.stage_ic(self.config.bkg_dir, self.runtime_config.DATA, self.runtime_config.gcyc)

        # swap date and stack size
        #domain_stack_size = os.getenv('DOMAIN_STACK_SIZE')
        domain_stack_size = self.config.DOMAIN_STACK_SIZE
        ymdhms = [int(s) for s in self.config.window_begin.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
        with open(self.config.mom_input_nml_tmpl, 'r') as nml_file:
            nml = f90nml.read(nml_file)
            nml['ocean_solo_nml']['date_init'] = ymdhms
            nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
            ufsda.disk_utils.removefile(self.config.mom_input_nml)
            nml.write(self.config.mom_input_nml)
        
 
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
