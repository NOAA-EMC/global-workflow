#!/usr/bin/env python3

import os
import datetime
from logging import getLogger
from wxflow import Task, cast_as_dtype, logit

from pygfs.jedi import Jedi

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
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'IAUHH': cast_as_dtype(task_config.IAUFHRS)
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Construct JEDI object
        self.jedi = JEDI(self.task_config, yaml_name)

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

        # Initialize FileHandler to make directories and copy files
        if task_config.DOIAU and task_config.l4densvar and task_config.lwrite4danl:
            fh_dict = {'mkdir': [],
                       'copy': []}

            for fh in task_config.IAUHH:
                if fh == 6:
                    # For full res analysis
                    CalcAnlDir = task_config.DATA + '/calcanl_' + format(fh, '02')

                    if not os.path.exists(CalcAnlDir):
                        fh_dict['mkdir'].append(CalcAnlDir)
                    fh_dict['copy'].append([task_config.CALCANLEXEC,
                                            CalcAnlDir + '/calc_anl.x'])
                    fh_dict['copy'].append([task_config.DATA + '/siginc.nc',
                                            CalcAnlDir + '/siginc.nc.06'])
                    fh_dict['copy'].append([task_config.DATA + '/sigf06',
                                            CalcAnlDir + '/ges.06'])
                    fh_dict['copy'].append([task_config.DATA + '/siganl',
                                            CalcAnlDir + '/anl.06'])
                    fh_dict['copy'].append([task_config.CHGRESINCEXEC,
                                            CalcAnlDir + '/chgres_inc.x'])

                    # For ensemble res analysis
                    if Run in ["gdas", "gfs"]:
                        CalcAnlDir = task_config.DATA + '/calcanl_ensres_' + format(fh, '02')

                        if not os.path.exists(CalcAnlDir):
                            fh_dict['mkdir'].append(CalcAnlDir)
                        fh_dict['copy'].append([task_config.CALCANLEXEC,
                                                CalcAnlDir + '/calc_anl.x'])
                        fh_dict['copy'].append([task_config.DATA + '/siginc.nc',
                                                CalcAnlDir + '/siginc.nc.06'])
                        fh_dict['copy'].append([task_config.COM_ATMOS_ANALYSIS + '/' + task_config.APREFIX + 'atmanl.ensres.nc',
                                                CalcAnlDir + '/anl.ensres.06'])
                        fh_dict['copy'].append([task_config.COM_ATMOS_HISTORY_PREV + '/' + task_config.GPREFIX + 'atmf006.ensres.nc',
                                                CalcAnlDir + '/ges.ensres.06'])
                        fh_dict['copy'].append([task_config.DATA + '/sigf06',
                                                CalcAnlDir + '/ges.06'])
                else:
                    if os.path.isfile('sigi' + format(fh, '02') + '.nc'):
                        # For full res analysis
                        CalcAnlDir = task_config.DATA + '/calcanl_' + format(fh, '02')
                        CalcAnlDir6 = task_config.DATA + '/calcanl_' + format(6, '02')

                        if not os.path.exists(CalcAnlDir):
                            fh_dict['mkdir'].append(CalcAnlDir)
                        if not os.path.exists(CalcAnlDir6):
                            fh_dict['mkdir'].append(CalcAnlDir6)
                        fh_dict['copy'].append([task_config.COM_ATMOS_ANALYSIS + '/' + task_config.APREFIX + 'atma' + format(fh, '03') + '.nc',
                                                CalcAnlDir6 + '/anl.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.DATA + '/siga' + format(fh, '02'),
                                                CalcAnlDir6 + '/anl.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.DATA + '/sigi' + format(fh, '02') + '.nc',
                                                CalcAnlDir + '/siginc.nc.' + format(fh, '02')])
                        fh_dict['copy'].append([CalcAnlDir6 + '/inc.fullres.' + format(fh, '02'),
                                                CalcAnlDir + '/inc.fullres.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.DATA + '/sigf' + format(fh, '02'),
                                                CalcAnlDir6 + '/ges.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.DATA + '/sigf' + format(fh, '02'),
                                                CalcAnlDir + '/ges.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.CHGRESINCEXEC,
                                                CalcAnlDir + '/chgres_inc.x'])

                        # For ensemble res analysis
                        CalcAnlDir = task_config.DATA + '/calcanl_ensres_' + format(fh, '02')
                        CalcAnlDir6 = task_config.DATA + '/calcanl_ensres_' + format(6, '02')
                        if not os.path.exists(CalcAnlDir):
                            fh_dict['mkdir'].append(CalcAnlDir)
                        if not os.path.exists(CalcAnlDir6):
                            fh_dict['mkdir'].append(CalcAnlDir6)
                        fh_dict['copy'].append([task_config.COM_ATMOS_ANALYSIS + '/' + task_config.APREFIX + 'atma' + format(fh, '03') + '.ensres.nc',
                                                CalcAnlDir6 + '/anl.ensres.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.DATA + '/sigi' + format(fh, '02') + '.nc',
                                                CalcAnlDir6 + '/siginc.nc.' + format(fh, '02')])
                        fh_dict['copy'].append([task_config.COM_ATMOS_HISTORY_PREV + '/' + task_config.GPREFIX + 'atmf' + format(fh, '03') + '.ensres.nc',
                                                CalcAnlDir6 + '/ges.ensres.' + format(fh, '02')])
        else:
            # For full res analysis
            CalcAnlDir = task_config.DATA + '/calcanl_' + format(6, '02')

            if not os.path.exists(CalcAnlDir):
                fh_dict['mkdir'].append(CalcAnlDir)
            fh_dict['copy'].append([task_config.CALCANLEXEC,
                                    CalcAnlDir + '/calc_anl.x'])
            fh_dict['copy'].append([task_config.DATA + '/siginc.nc',
                                    CalcAnlDir + '/siginc.nc.06'])
            fh_dict['copy'].append([task_config.DATA + '/sigf06',
                                    CalcAnlDir + '/ges.06'])
            fh_dict['copy'].append([task_config.DATA + '/siganl',
                                    CalcAnlDir + '/anl.06'])
            fh_dict['copy'].append([task_config.CHGRESINCEXEC,
                                    CalcAnlDir + '/chgres_inc.x'])

            # For ensemble res analysis
            CalcAnlDir = task_config.DATA + '/calcanl_ensres_' + format(6, '02')

            if not os.path.exists(CalcAnlDir):
                fh_dict['mkdir'].append(CalcAnlDir)
            fh_dict['copy'].append([task_config.CALCANLEXEC,
                                    CalcAnlDir + '/calc_anl.x'])
            fh_dict['copy'].append([task_config.DATA + '/siginc.nc',
                                    CalcAnlDir + '/siginc.nc.06'])
            fh_dict['copy'].append([task_config.COM_ATMOS_ANALYSIS + '/' + APrefix + 'atmanl.ensres.nc',
                                    CalcAnlDir + '/anl.ensres.06'])
            fh_dict['copy'].append([task_config.COM_ATMOS_HISTORY_PREV + '/' + GPrefix + 'atmf006.ensres.nc',
                                    CalcAnlDir + '/ges.ensres.06'])

        # Stage files
        FileHandler(fh_dict).sync()

    @logit(logger)
    def execute(self, aprun_cmd: str) -> None:
        self.jedi.execute(self.task_config, aprun_cmd)
