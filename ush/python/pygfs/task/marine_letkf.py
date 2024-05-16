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
                    Task,
                    WorkflowException)

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


        FileHandler({'mkdir': [self.config.bkg_dir]}).sync()
        bkg_utils.gen_bkg_list(bkg_path=self.config.COM_OCEAN_HISTORY_PREV,
                               out_path=self.config.bkg_dir,
                               window_begin=self.config.window_begin,
                               yaml_name=self.config.BKG_LIST)

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
