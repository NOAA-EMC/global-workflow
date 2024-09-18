#!/usr/bin/env python3

import datetime
from logging import getLogger
from pprint import pformat
import os
from pygfs.jedi import Jedi
from wxflow import add_to_datetime, AttrDict, FileHandler, logit, parse_j2yaml, Task, save_as_yaml, to_fv3time, to_timedelta

logger = getLogger(__name__.split('.')[-1])


class CalcAnalysis(Task):
    """
    Class for JEDI-based analysis calculation
    """
    @logit(logger, name="CalcAnalysis")
    def __init__(self, config, yaml_name=None):
        super().__init__(config)

        _res = int(self.task_config.CASE[1:])
        _res_anl = int(self.task_config.CASE_ANL[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'CalcAnlDir': lambda fh : f"{self.task_config.DATA}/calcanl_{format(fh, '02')}"
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Construct JEDI object
        self.jedi = Jedi(self.task_config, yaml_name)

    @logit(logger)
    def initialize_jedi(self) -> None:
        # get JEDI-to-FV3 increment converter config and save to YAML file
        logger.info(f"Generating JEDI YAML config: {self.jedi.yaml}")
        self.jedi.set_config(self.task_config)
        logger.debug(f"JEDI config:\n{pformat(self.jedi.config)}")

        # save JEDI config to YAML file
        logger.debug(f"Writing JEDI YAML config to: {self.jedi.yaml}")
        save_as_yaml(self.jedi.config, self.jedi.yaml)

        # stage fix files
        if not os.path.isdir(self.task_config.DATA + 'fv3jedi'):
            logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
            jedi_fix_dict = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
            FileHandler(jedi_fix_dict).sync()
            logger.debug(f"JEDI fix files:\n{pformat(jedi_fix_dict)}")

        # link JEDI executable
        logger.info(f"Linking JEDI executable {self.task_config.JEDIEXE} to {self.jedi.exe}")
        self.jedi.link_exe(self.task_config)

    @logit(logger)
    def initialize(self) -> None:
        # Initialize dictionary used to construct Filehandler
        fh_dict = {'mkdir': [],
                   'copy': []}

        # Initialize FileHandler to make directories and copy files
        hist_prefix = f"{self.task_config.COM_ATMOS_HISTORY_PREV}/{self.task_config.GPREFIX}"
        anl_prefix = f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}"
        for fh in self.task_config.IAUFHRS:
            fh_dict['mkdir'].append(self.task_config.CalcAnlDir(fh))

            fh_dict['copy'].append([f"{hist_prefix}cubed_sphere_grid_atmf{format(fh, '03')}.nc",
                                    f"{self.task_config.CalcAnlDir(fh)}/ges.atm.{format(fh, '02')}.nc"])
            fh_dict['copy'].append([f"{hist_prefix}cubed_sphere_grid_sfcf{format(fh, '03')}.nc",
                                    f"{self.task_config.CalcAnlDir(fh)}/ges.sfc.{format(fh, '02')}.nc"])
            
            if fh == 6:
                for itile in range(6):
                    fh_dict['copy'].append([f"{anl_prefix}atminc.tile{itile+1}.nc",
                                            f"{self.task_config.CalcAnlDir(fh)}/siginc.06.tile{itile+1}.nc"])
            else:
                for itile in range(6):
                    fh_dict['copy'].append([f"{anl_prefix}/atmi{format(fh, '02')}.tile{itile+1}.nc",
                                            f"{self.task_config.CalcAnlDir(fh)}/siginc.{format(fh, '02')}.tile{itile+1}.nc"])

        # Stage files
        FileHandler(fh_dict).sync()

    @logit(logger)
    def finalize(self) -> None:
        cdate = to_fv3time(self.task_config.current_cycle).replace('.', '_')
        anl_prefix = f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}"
        
        # Initialize dictionary used to construct Filehandler
        fh_dict = {'mkdir': [],
                   'copy': []}
        
        for fh in self.task_config.IAUFHRS:
            if fh == 6:
                fh_dict['copy'].append([f"{self.task_config.CalcAnlDir(fh)}/anl.{format(fh, '02')}.{cdate}z.nc4",
                                        f"{anl_prefix}atmanl.nc"])
            else:
                fh_dict['copy'].append([f"{self.task_config.CalcAnlDir(fh)}/anl.{format(fh, '02')}.{cdate}z.nc4",
                                        f"{anl_prefix}atma{format(fh, '03')}.nc"])

        FileHandler(fh_dict).sync()
                                   
    @logit(logger)
    def execute(self, aprun_cmd: str) -> None:
        self.jedi.execute(self.task_config, aprun_cmd)
