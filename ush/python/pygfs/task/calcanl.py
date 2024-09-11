#!/usr/bin/env python3

import datetime
from logging import getLogger
from pprint import pformat
import os
from pygfs.jedi import Jedi
from wxflow import add_to_datetime, AttrDict, FileHandler, logit, Task, save_as_yaml, to_timedelta

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

        # link JEDI executable
        logger.info(f"Linking JEDI executable {self.task_config.JEDIEXE} to {self.jedi.exe}")
        self.jedi.link_exe(self.task_config)

    @logit(logger)
    def initialize(self) -> None:
        logger.info('calcanl_gfs beginning at: ', datetime.datetime.utcnow())

        # Initialize dictionary used to construct Filehandler
        fh_dict = {'mkdir': [],
                   'copy': []}

        logger.info(f"Linking JEDI executable {self.task_config.JEDIEXE} to {self.jedi.exe}")
        self.jedi.link_exe(self.task_config)
        
        # Initialize FileHandler to make directories and copy files
        if self.task_config.DOIAU and self.task_config.l4densvar and self.task_config.lwrite4danl:

            for fh in self.task_config.IAUFHRS:
                if fh == 6:
                    CalcAnlDir = self.task_config.DATA + '/calcanl_' + format(fh, '02')

                    if not os.path.exists(CalcAnlDir):
                        fh_dict['mkdir'].append(CalcAnlDir)
                    fh_dict['copy'].append([self.task_config.DATA + '/siginc.nc',
                                            CalcAnlDir + '/siginc.nc.06'])
                    fh_dict['copy'].append([self.task_config.DATA + '/sigf06',
                                            CalcAnlDir + '/ges.06'])
                    fh_dict['copy'].append([self.task_config.DATA + '/siganl',
                                            CalcAnlDir + '/anl.06'])
                else:
                    if os.path.isfile('sigi' + format(fh, '02') + '.nc'):
                        CalcAnlDir = self.task_config.DATA + '/calcanl_' + format(fh, '02')
                        CalcAnlDir6 = self.task_config.DATA + '/calcanl_' + format(6, '02')

                        if not os.path.exists(CalcAnlDir):
                            fh_dict['mkdir'].append(CalcAnlDir)
                        if not os.path.exists(CalcAnlDir6):
                            fh_dict['mkdir'].append(CalcAnlDir6)
                        fh_dict['copy'].append([self.task_config.COM_ATMOS_ANALYSIS + '/' + self.task_config.APREFIX + 'atma' + format(fh, '03') + '.nc',
                                                CalcAnlDir6 + '/anl.' + format(fh, '02')])
                        fh_dict['copy'].append([self.task_config.DATA + '/siga' + format(fh, '02'),
                                                CalcAnlDir6 + '/anl.' + format(fh, '02')])
                        fh_dict['copy'].append([self.task_config.DATA + '/sigi' + format(fh, '02') + '.nc',
                                                CalcAnlDir + '/siginc.nc.' + format(fh, '02')])
                        fh_dict['copy'].append([CalcAnlDir6 + '/inc.fullres.' + format(fh, '02'),
                                                CalcAnlDir + '/inc.fullres.' + format(fh, '02')])
                        fh_dict['copy'].append([self.task_config.DATA + '/sigf' + format(fh, '02'),
                                                CalcAnlDir6 + '/ges.' + format(fh, '02')])
                        fh_dict['copy'].append([self.task_config.DATA + '/sigf' + format(fh, '02'),
                                                CalcAnlDir + '/ges.' + format(fh, '02')])
        else:
            CalcAnlDir = self.task_config.DATA + '/calcanl_' + format(6, '02')

            if not os.path.exists(CalcAnlDir):
                fh_dict['mkdir'].append(CalcAnlDir)
            fh_dict['copy'].append([self.task_config.COM_ATMOS_ANALYSIS + '/' + self.task_config.APREFIX + 'atminc006.nc',
                                    CalcAnlDir + '/siginc.nc.06'])
            fh_dict['copy'].append([self.task_config.COM_ATMOS_HISTORY_PREV + '/' + self.task_config.GPREFIX + 'cubed_sphere_grid_atmf006.nc'
                                    CalcAnlDir + '/ges.06'])

        # Stage files
        FileHandler(fh_dict).sync()

    @logit(logger)
    def execute(self, aprun_cmd: str) -> None:
        self.jedi.execute(self.task_config, aprun_cmd)
