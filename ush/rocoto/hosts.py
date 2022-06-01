#!/usr/bin/env python3

import os

__all__ = ['Host']


class Host:
    """
    Gather Host specific information.
    Someday the info will be pushed out of the code and into a yaml or something.
    """

    # SUPPORTED_HOSTS = ['HERA', 'ORION', 'JET',
    #                   'WCOSS_C', 'WCOSS_DELL_P3', 'WCOSS2']
    SUPPORTED_HOSTS = ['HERA', 'ORION',
                       'WCOSS_DELL_P3']  # TODO - Remove this and uncomment above line

    def __init__(self):

        self.machine = self.detect
        self.scheduler = self.get_scheduler
        self.info = self._get_info

    @classmethod
    @property
    def detect(cls):

        # machine = 'NOTFOUND'  # TODO - Uncomment this line
        machine = 'ORION'  # TODO - Remove this line

        if os.path.exists('/scratch1/NCEPDEV'):
            machine = 'HERA'
        elif os.path.exists('/work/noaa'):
            machine = 'ORION'
        elif os.path.exists('/lfs4/HFIP'):
            machine = 'JET'
        elif os.path.exists('/gpfs') and os.path.exists('/etc/SuSE-release'):
            machine = 'WCOSS_C'
        elif os.path.exists('/gpfs/dell2'):
            machine = 'WCOSS_DELL_P3'
        elif os.path.exists('/lfs/f1'):
            machine = 'WCOSS2'

        if machine not in Host.SUPPORTED_HOSTS:
            raise NotImplementedError(f'This machine is not a supported host.\n' +
                                      'Currently supported hosts are:\n' +
                                      f'{" | ".join(Host.SUPPORTED_HOSTS)}')

        return machine

    @classmethod
    @property
    def get_scheduler(cls) -> str:

        SCHEDULER_MAP = {'HERA': 'slurm',
                         'JET': 'slurm',
                         'ORION': 'slurm',
                         'WCOSS_C': 'lsfcray',
                         'WCOSS_DELL_P3': 'lsf',
                         'WCOSS_DELL_P3p5': 'lsf',
                         'WCOSS2': 'pbspro'}

        machine = Host.detect
        try:
            return SCHEDULER_MAP[machine.upper()]
        except KeyError:
            raise NotImplementedError(f'{machine} is not a supported host.\n' +
                                      'Currently supported hosts are:\n' +
                                      f'{" | ".join(Host.SUPPORTED_HOSTS)}')

    @property
    def _get_info(self) -> dict:

        HOST_INFO_MAP = {
            'HERA': self._hera(),
            'ORION': self._orion(),
            'JET': self._jet(),
            'WCOSS_C': self._wcoss_c(),
            'WCOSS_DELL_P3': self._wcoss_dell_p3(),
            'WCOSS_DELL_P3p5': self._wcoss_dell_p3p5(),
            'WCOSS2': self._wcoss2()}

        try:
            info = HOST_INFO_MAP[self.machine.upper()]
        except KeyError as exc:
            raise NotImplementedError(f'{self.machine} is not a supported host.\n' +
                                      'Currently supported hosts are:\n' +
                                      f'{" | ".join(Host.SUPPORTED_HOSTS)}')

        return info

    @staticmethod
    def _hera() -> dict:

        info = {
            'base_git': '/scratch1/NCEPDEV/global/glopara/git',
            'base_svn': '/scratch1/NCEPDEV/global/glopara/svn',
            'dmpdir': '/scratch1/NCEPDEV/global/glopara/dump',
            'nwprod': '/scratch1/NCEPDEV/global/glopara/nwpara',
            'comroot': '/scratch1/NCEPDEV/global/glopara/com',
            'homedir': '/scratch1/NCEPDEV/global/$USER',
            'stmp': '/scratch1/NCEPDEV/stmp2/$USER',
            'ptmp': '/scratch1/NCEPDEV/stmp4/$USER',
            'noscrub': '$HOMEDIR',
            'account': 'fv3-cpu',
            'queue': 'batch',
            'queue_service': 'service',
            'partition_batch': 'hera',
            'chgrp_rstprod': 'YES',
            'chgrp_cmd': 'chgrp rstprod',
            'hpssarch': 'YES',
            'localarch': 'NO',
            'atardir': '/NCEPDEV/$HPSS_PROJECT/1year/$USER/$machine/scratch/$PSLOT',
        }

        return info

    @staticmethod
    def _orion() -> dict:

        info = {
            'base_git': '/work/noaa/global/glopara/git',
            'base_svn': '/work/noaa/global/glopara/svn',
            'dmpdir': '/work/noaa/rstprod/dump',
            'nwprod': '/work/noaa/global/glopara/nwpara',
            'comroot': '/work/noaa/global/glopara/com',
            'homedir': '/work/noaa/global/$USER',
            'stmp': '/work/noaa/stmp/$USER',
            'ptmp': '/work/noaa/stmp/$USER',
            'noscrub': '$HOMEDIR',
            'account': 'fv3-cpu',
            'queue': 'batch',
            'queue_service': 'service',
            'partition_batch': 'orion',
            'chgrp_rstprod': 'YES',
            'chgrp_cmd': 'chgrp rstprod',
            'hpssarch': 'NO',
            'localarch': 'NO',
            'atardir': '$NOSCRUB/archive_rotdir/$PSLOT',
        }

        return info

    @staticmethod
    def _jet() -> dict:

        info = {}

        return info

    @staticmethod
    def _wcoss_c() -> dict:

        info = {
            'base_git': '/gpfs/hps3/emc/global/noscrub/emc.glopara/git',
            'base_svn': '/gpfs/hps3/emc/global/noscrub/emc.glopara/svn',
            'dmpdir': '/gpfs/dell3/emc/global/dump',
            'nwprod': '${NWROOT:-"/gpfs/hps/nco/ops/nwprod"}',
            'comroot': '${COMROOT:-"/gpfs/hps/nco/ops/com"}',
            'homedir': '/gpfs/hps3/emc/global/noscrub/$USER',
            'stmp': '/gpfs/hps2/stmp/$USER',
            'ptmp': '/gpfs/hps2/ptmp/$USER',
            'noscrub': '/gpfs/hps3/emc/global/noscrub/$USER',
            'account': 'GFS-DEV',
            'queue': 'dev',
            'queue_service': 'dev_transfer',
            'chgrp_rstprod': 'YES',
            'chgrp_cmd': 'chgrp rstprod',
            'hpssarch': 'YES',
            'localarch': 'NO',
            'atardir': '/NCEPDEV/$HPSS_PROJECT/1year/$USER/$machine/scratch/$PSLOT',
        }

        return info

    @staticmethod
    def _wcoss_dell_p3() -> dict:

        info = {
            'base_git': '/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git',
            'base_svn': '/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git',
            'dmpdir': '/gpfs/dell3/emc/global/dump',
            'nwprod': '${NWROOT:-"/gpfs/dell1/nco/ops/nwprod"}',
            'comroot': '${COMROOT:-"/gpfs/dell1/nco/ops/com"}',
            'homedir': '/gpfs/dell2/emc/modeling/noscrub/$USER',
            'stmp': '/gpfs/dell3/stmp/$USER',
            'ptmp': '/gpfs/dell3/ptmp/$USER',
            'noscrub': '$HOMEDIR',
            'account': 'GFS-DEV',
            'queue': 'dev',
            'queue_service': 'dev_transfer',
            'partition_batch': None,
            'chgrp_rstprod': 'YES',
            'chgrp_cmd': 'chgrp rstprod',
            'hpssarch': 'YES',
            'localarch': 'NO',
            'atardir': '/NCEPDEV/$HPSS_PROJECT/1year/$USER/$machine/scratch/$PSLOT',
        }

        return info

    @staticmethod
    def _wcoss_dell_p3p5() -> dict:

        info = {
            'base_git': '/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git',
            'base_svn': '/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git',
            'dmpdir': '/gpfs/dell3/emc/global/dump',
            'nwprod': '${NWROOT:-"/gpfs/dell1/nco/ops/nwprod"}',
            'comroot': '${COMROOT:-"/gpfs/dell1/nco/ops/com"}',
            'homedir': '/gpfs/dell2/emc/modeling/noscrub/$USER',
            'stmp': '/gpfs/dell3/stmp/$USER',
            'ptmp': '/gpfs/dell3/ptmp/$USER',
            'noscrub': '$HOMEDIR',
            'account': 'GFS-DEV',
            'queue': 'dev2',
            'queue_service': 'dev2_transfer',
            'partition_batch': None,
            'chgrp_rstprod': 'YES',
            'chgrp_cmd': 'chgrp rstprod',
            'hpssarch': 'YES',
            'localarch': 'NO',
            'atardir': '/NCEPDEV/$HPSS_PROJECT/1year/$USER/$machine/scratch/$PSLOT',
        }

        return info

    @staticmethod
    def _wcoss2() -> dict:

        info = {}

        return info
